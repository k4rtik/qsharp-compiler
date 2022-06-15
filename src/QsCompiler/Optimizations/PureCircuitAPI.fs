// Copyright (c) Microsoft Corporation. All rights reserved.
// Licensed under the MIT License.

module Microsoft.Quantum.QsCompiler.Experimental.PureCircuitAPI

open System
open System.Collections.Immutable
open Microsoft.Quantum.QsCompiler.DataTypes
open Microsoft.Quantum.QsCompiler.Experimental.Utils
open Microsoft.Quantum.QsCompiler.SyntaxTokens
open Microsoft.Quantum.QsCompiler.SyntaxTree


/// Any constant expression
type Literal =
    | IntLiteral of int64
    | DoubleLiteral of double
    | PauliLiteral of QsPauli
    | PauliArray of ImmutableArray<QsPauli>

/// Any expression
type Expression =
    | Literal of Literal
    | Tuple of ImmutableArray<Expression>
    | Qubit of int
    | QubitArray of ImmutableArray<int>
    | UnknownValue of int

/// A call to a quantum gate, with the given functors and arguments
type GateCall =
    {
        Gate: QsQualifiedName
        Info: CallableInformation
        Adjoint: bool
        Controls: ImmutableArray<int>
        Arg: Expression
    }

/// A pure quantum circuit with arbitrarily many (non-qubit) parameters
type Circuit =
    {
        NumQubits: int
        NumUnknownValues: int
        Gates: ImmutableArray<GateCall>
    }

/// Metadata used for constructing circuits.
/// Currently just stores the variable names corresponding to each qubit reference.
/// In the future, this will include a map from all symbols to their Q# representation.
type private CircuitContext =
    {
        Callables: ImmutableDictionary<QsQualifiedName, QsCallable>
        DistinctNames: Set<string>
        Qubits: ImmutableArray<TypedExpression>
        UnknownValues: ImmutableArray<TypedExpression>
    }


/// Converts a TypedExpression to an Expression
let rec private toExpression (cc: CircuitContext, expr: TypedExpression) : (CircuitContext * Expression) option =
    let someLiteral a = Some(cc, Literal a)

    let typeIsArray k =
        expr.ResolvedType.Resolution = TypeKind.ArrayType(ResolvedType.New k)

    let ensureMatchingIndex (l: ImmutableArray<_>) =
        let existing = Seq.indexed l |> Seq.tryPick (fun (i, ex) -> if expr = ex then Some(l, i) else None)
        let newList, index = existing |? (l.Add expr, l.Length)

        if index > (1 <<< 29) then ArgumentException "Trying to create too large of a circuit" |> raise

        newList, index

    let recurse cc seq f g =
        maybe {
            let ccRef = ref cc
            let outputRef = ref []

            for sub in seq do
                let! ccNew, exprNew = toExpression (!ccRef, sub)
                let! toAdd = f exprNew
                ccRef := ccNew
                outputRef := !outputRef @ [ toAdd ]

            return !ccRef, g (ImmutableArray.CreateRange(!outputRef))
        }

    let rec mightContainQubit =
        function
        | TypeKind.Qubit -> true
        | TypeKind.ArrayType t -> mightContainQubit t.Resolution
        | TypeKind.TupleType ts -> ts |> Seq.exists (fun t -> mightContainQubit t.Resolution)
        | TypeKind.UserDefinedType u ->
            let qualName = { Namespace = u.Namespace; Name = u.Name }
            mightContainQubit (cc.Callables.[qualName]).Signature.ArgumentType.Resolution
        | _ -> false

    match expr.Expression with
    | ExprKind.IntLiteral x -> IntLiteral x |> someLiteral
    | ExprKind.DoubleLiteral x -> DoubleLiteral x |> someLiteral
    | ExprKind.PauliLiteral x -> PauliLiteral x |> someLiteral
    | ExprKind.ValueTuple x -> recurse cc x Some Tuple
    | ExprKind.ValueArray x when typeIsArray TypeKind.Pauli ->
        recurse
            cc
            x
            (function
            | Literal (PauliLiteral p) -> Some p
            | _ -> None)
            (PauliArray >> Literal)
    | ExprKind.ValueArray x when typeIsArray TypeKind.Qubit ->
        recurse
            cc
            x
            (function
            | Qubit i -> Some i
            | _ -> None)
            QubitArray
    | ExprKind.Identifier (LocalVariable name, _) when
        expr.ResolvedType.Resolution = TypeKind.Qubit && cc.DistinctNames.Contains name
        ->
        let newQubits, i = ensureMatchingIndex cc.Qubits
        Some({ cc with Qubits = newQubits }, Qubit i)
    | _ when mightContainQubit expr.ResolvedType.Resolution -> None
    | _ ->
        let newUnknownValues, i = ensureMatchingIndex cc.UnknownValues
        Some({ cc with UnknownValues = newUnknownValues }, UnknownValue i)

/// Converts a TypedExpression to a GateCall
let private toGateCall (cc: CircuitContext, expr: TypedExpression) : (CircuitContext * GateCall) option =
    let rec helper cc method arg : (CircuitContext * GateCall) option =
        maybe {
            match method.Expression with
            | AdjointApplication x ->
                let! cc, result = helper cc x arg
                return cc, { result with Adjoint = not result.Adjoint }
            | ControlledApplication x ->
                match arg.Expression with
                | ExprKind.ValueTuple vt ->
                    do! check (vt.Length = 2)
                    let! cc, controlsExpr = toExpression (cc, vt.[0])

                    let! controlQubits =
                        match controlsExpr with
                        | QubitArray i -> Some i
                        | _ -> None

                    let! cc, result = helper cc x vt.[1]
                    return cc, { result with Controls = controlQubits.AddRange result.Controls }
                | _ -> return! None
            | Identifier (GlobalCallable name, _) ->
                let! cc, argVal = toExpression (cc, arg)
                let info = (cc.Callables.[name]).Signature.Information

                return
                    cc,
                    {
                        Gate = name
                        Info = info
                        Adjoint = false
                        Controls = ImmutableArray.Empty
                        Arg = argVal
                    }
            | _ -> return! None
        }

    match expr.Expression with
    | CallLikeExpression (method, arg) -> helper cc method arg
    | _ -> None

/// Converts a list of TypedExpressions to a Circuit, CircuitContext pair.
/// Returns None if the given expression list cannot be converted to a pure circuit.
let private toCircuit callables distinctNames exprList : (Circuit * CircuitContext) option =
    maybe {
        let ccRef =
            ref
                {
                    Callables = callables
                    DistinctNames = distinctNames
                    Qubits = ImmutableArray.Empty
                    UnknownValues = ImmutableArray.Empty
                }

        let outputRef = ref []

        for expr in exprList do
            let! ccNew, gate = toGateCall (!ccRef, expr)
            ccRef := ccNew
            outputRef := !outputRef @ [ gate ]

        let circuit =
            {
                NumQubits = (!ccRef).Qubits.Length
                NumUnknownValues = (!ccRef).UnknownValues.Length
                Gates = ImmutableArray.CreateRange !outputRef
            }

        return circuit, !ccRef
    }


/// Returns the Q# expression corresponding to the given Expression
let rec private fromExpression (cc: CircuitContext) (expr: Expression) : TypedExpression =
    let buildArray t x =
        x |> ImmutableArray.CreateRange |> ExprKind.ValueArray |> wrapExpr (ArrayType(ResolvedType.New t))

    let rec getType =
        function
        | Literal (IntLiteral _) -> ResolvedType.New Int
        | Literal (DoubleLiteral _) -> ResolvedType.New Double
        | Literal (PauliLiteral _) -> ResolvedType.New Pauli
        | Literal (PauliArray _) -> ResolvedType.New(ArrayType(ResolvedType.New Pauli))
        | Tuple x -> ResolvedType.New(TupleType(x |> Seq.map getType |> ImmutableArray.CreateRange))
        | Qubit _ -> ResolvedType.New TypeKind.Qubit
        | QubitArray _ -> ResolvedType.New(ArrayType(ResolvedType.New TypeKind.Qubit))
        | UnknownValue i -> cc.UnknownValues.[i].ResolvedType

    match expr with
    | Literal (IntLiteral x) -> ExprKind.IntLiteral x |> wrapExpr Int
    | Literal (DoubleLiteral x) -> ExprKind.DoubleLiteral x |> wrapExpr Double
    | Literal (PauliLiteral x) -> ExprKind.PauliLiteral x |> wrapExpr Pauli
    | Literal (PauliArray x) -> x |> Seq.map (ExprKind.PauliLiteral >> wrapExpr Pauli) |> buildArray Pauli
    | Tuple x ->
        x
        |> Seq.map (fromExpression cc)
        |> ImmutableArray.CreateRange
        |> ExprKind.ValueTuple
        |> wrapExpr (getType expr).Resolution
    | Qubit i -> cc.Qubits.[i]
    | QubitArray x -> x |> Seq.map (fun i -> cc.Qubits.[i]) |> buildArray TypeKind.Qubit
    | UnknownValue i -> cc.UnknownValues.[i]

/// Returns the Q# expression correponding to the given gate call
let private fromGateCall (cc: CircuitContext) (gc: GateCall) : TypedExpression =
    let mutable arg = fromExpression cc gc.Arg

    let methodType =
        TypeKind.Operation((arg.ResolvedType, ResolvedType.New UnitType), CallableInformation.NoInformation)

    let mutable method = wrapExpr methodType (Identifier(GlobalCallable gc.Gate, Null))

    if gc.Adjoint then method <- wrapExpr methodType (AdjointApplication method)

    if not gc.Controls.IsEmpty then
        let argType =
            TupleType(
                ImmutableArray.Create(ResolvedType.New(ArrayType(ResolvedType.New TypeKind.Qubit)), arg.ResolvedType)
            )

        arg <- wrapExpr argType (ValueTuple(ImmutableArray.Create(fromExpression cc (QubitArray gc.Controls), arg)))

        let methodType =
            TypeKind.Operation((ResolvedType.New argType, ResolvedType.New UnitType), CallableInformation.NoInformation)

        method <- wrapExpr methodType (ControlledApplication method)

    wrapExpr UnitType (CallLikeExpression(method, arg))

/// Returns the list of Q# expressions corresponding to the given circuit
let private fromCircuit (cc: CircuitContext) (circuit: Circuit) =
    Seq.map (fromGateCall cc) circuit.Gates |> ImmutableArray.CreateRange


/// Given a pure circuit, performs basic optimizations and returns the new circuit
let private optimizeCircuit (circuit: Circuit) : Circuit option =
    let mutable circuit = circuit
    let mutable i = 0

    while i < circuit.Gates.Length - 1 do
        if circuit.Gates.[i] = { circuit.Gates.[i + 1] with Adjoint = not circuit.Gates.[i + 1].Adjoint } then
            circuit <- { circuit with Gates = removeIndices [ i; i + 1 ] circuit.Gates |> ImmutableArray.CreateRange }
        elif circuit.Gates.[i] = circuit.Gates.[i + 1]
             && circuit.Gates.[i].Info.InferredInformation.IsSelfAdjoint then
            circuit <- { circuit with Gates = removeIndices [ i; i + 1 ] circuit.Gates |> ImmutableArray.CreateRange }
        else
            i <- i + 1

    Some circuit


/// Given a list of Q# expressions, tries to convert it to a pure circuit.
/// If this succeeds, optimizes the circuit and returns the new list of Q# expressions.
/// Otherwise, returns the given list.
let internal optimizeExprList callables distinctNames exprList =
    (* let s = List.map (fun x -> printExpr x.Expression) exprList
    if exprList.Length >= 5 then
        printfn "Optimizing %O" s*)
    maybe {
        let! circuit, cc = toCircuit callables distinctNames exprList

        try
            let! newCircuit = optimizeCircuit circuit
            return fromCircuit cc newCircuit
        with
        | _ -> return! None
    }
    |? exprList
