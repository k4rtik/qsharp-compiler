// Copyright (c) Microsoft Corporation.
// Licensed under the MIT License.

namespace Microsoft.Quantum.QsCompiler

open System

[<CustomComparison>]
[<CustomEquality>]
type CaseInsensitive =
    | CaseInsensitive of string

    member ci.Original =
        let (CaseInsensitive s) = ci
        s

    override ci.Equals obj = (ci :> IComparable).CompareTo obj = 0

    override ci.GetHashCode() =
        StringComparer.InvariantCultureIgnoreCase.GetHashCode ci.Original

    interface IComparable with
        member ci.CompareTo obj =
            match obj with
            | :? CaseInsensitive as other ->
                String.Compare(ci.Original, other.Original, StringComparison.InvariantCultureIgnoreCase)
            | _ -> ArgumentException("Type mismatch.", nameof obj) |> raise

module CaseInsensitive =
    let original (CaseInsensitive s) = s

type ResultOpacity =
    | Opaque
    | Controlled
    | Transparent

module ResultOpacity =
    [<CompiledName "Opaque">]
    let opaque = Opaque

    [<CompiledName "Controlled">]
    let controlled = Controlled

    [<CompiledName "Transparent">]
    let transparent = Transparent

    let names =
        Map [ CaseInsensitive "Opaque", Opaque
              CaseInsensitive "Controlled", Controlled
              CaseInsensitive "Transparent", Transparent ]

    let ofName name =
        Map.tryFind (CaseInsensitive name) names

[<NoComparison>]
type ClassicalCompute =
    | Empty
    | Integral
    | Full

module ClassicalCompute =
    [<CompiledName "Empty">]
    let empty = Empty

    [<CompiledName "Integral">]
    let integral = Integral

    [<CompiledName "Full">]
    let full = Full

    let subsumes c1 c2 =
        match c1, c2 with
        | Full, _
        | Integral, Integral
        | _, Empty -> true
        | _, Full
        | Empty, _ -> false

    let merge c1 c2 =
        match c1, c2 with
        | Full, _
        | _, Full -> Full
        | Integral, Integral -> Integral
        | c, Empty
        | Empty, c -> c

    let names =
        Map [ CaseInsensitive "Empty", Empty
              CaseInsensitive "Integral", Integral
              CaseInsensitive "Full", Full ]

    let ofName name =
        Map.tryFind (CaseInsensitive name) names

[<NoComparison>]
type TargetCapability =
    | TargetCapability of resultOpacity: ResultOpacity * classicalCompute: ClassicalCompute

    member capability.ResultOpacity =
        let (TargetCapability (resultOpacity = opacity)) = capability
        opacity

    member capability.ClassicalCompute =
        let (TargetCapability (classicalCompute = classical)) = capability
        classical

module TargetCapability =
    [<CompiledName "Top">]
    let top = TargetCapability(Transparent, Full)

    [<CompiledName "Bottom">]
    let bottom = TargetCapability(Opaque, Empty)

    [<CompiledName "BasicExecution">]
    let basicExecution = TargetCapability(Opaque, Empty)

    [<CompiledName "AdaptiveExecution">]
    let adaptiveExecution = TargetCapability(Transparent, Integral)

    [<CompiledName "BasicQuantumFunctionality">]
    let basicQuantumFunctionality = TargetCapability(Opaque, Full)

    [<CompiledName "BasicMeasurementFeedback">]
    let basicMeasurementFeedback = TargetCapability(Controlled, Full)

    [<CompiledName "FullComputation">]
    let fullComputation = TargetCapability(Transparent, Full)

    [<CompiledName "Subsumes">]
    let subsumes (c1: TargetCapability) (c2: TargetCapability) =
        c1.ResultOpacity >= c2.ResultOpacity
        && ClassicalCompute.subsumes c1.ClassicalCompute c2.ClassicalCompute

    [<CompiledName "Merge">]
    let merge (c1: TargetCapability) (c2: TargetCapability) =
        TargetCapability(
            max c1.ResultOpacity c2.ResultOpacity,
            ClassicalCompute.merge c1.ClassicalCompute c2.ClassicalCompute
        )

    [<CompiledName "WithResultOpacity">]
    let withResultOpacity opacity (capability: TargetCapability) =
        TargetCapability(opacity, capability.ClassicalCompute)

    [<CompiledName "WithClassicalCompute">]
    let withClassicalCompute classical (capability: TargetCapability) =
        TargetCapability(capability.ResultOpacity, classical)

    let names =
        Map [ CaseInsensitive "BasicExecution", basicExecution
              CaseInsensitive "AdaptiveExecution", adaptiveExecution
              CaseInsensitive "BasicQuantumFunctionality", basicQuantumFunctionality
              CaseInsensitive "BasicMeasurementFeedback", basicMeasurementFeedback
              CaseInsensitive "FullComputation", fullComputation ]

    [<CompiledName "Name">]
    let name capability =
        Map.tryFindKey (fun _ -> (=) capability) names |> Option.map CaseInsensitive.original

    [<CompiledName "FromName">]
    let ofName name =
        Map.tryFind (CaseInsensitive name) names

type TargetCapability with
    member capability.Name = TargetCapability.name capability |> Option.toObj

    static member TryParse name =
        TargetCapability.ofName name |> Option.defaultValue Unchecked.defaultof<_>
