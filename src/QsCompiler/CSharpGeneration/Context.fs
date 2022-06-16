// Copyright (c) Microsoft Corporation. All rights reserved.
// Licensed under the MIT License.

namespace Microsoft.Quantum.QsCompiler.CsharpGeneration

open System.Collections.Generic
open System.Collections.Immutable
open System.Linq

open Microsoft.Quantum.QsCompiler
open Microsoft.Quantum.QsCompiler.DataTypes
open Microsoft.Quantum.QsCompiler.ReservedKeywords
open Microsoft.Quantum.QsCompiler.SyntaxTree
open Microsoft.Quantum.QsCompiler.Transformations.Core


module internal DeclarationLocations =

    type TransformationState() =

        member val internal CurrentSource = null with get, set
        member val internal DeclarationLocations = new List<string * Position>()

    type NamespaceTransformation(parent: SyntaxTreeTransformation<TransformationState>) =
        inherit NamespaceTransformation<TransformationState>(parent)

        override this.OnSource file =
            this.SharedState.CurrentSource <- Source.assemblyOrCodeFile file
            file

        override this.OnLocation sourceLocation =
            match sourceLocation with
            | Value (loc: QsLocation) when isNull this.SharedState.CurrentSource |> not ->
                this.SharedState.DeclarationLocations.Add(this.SharedState.CurrentSource, loc.Offset)
            | _ -> ()

            sourceLocation


    type internal SyntaxTreeTransformation() as this =
        inherit SyntaxTreeTransformation<TransformationState>(TransformationState(), TransformationOptions.NoRebuild)

        do
            this.Namespaces <- NamespaceTransformation(this)
            this.Statements <- StatementTransformation<TransformationState>(this, TransformationOptions.Disabled)
            this.Expressions <- ExpressionTransformation<TransformationState>(this, TransformationOptions.Disabled)
            this.Types <- TypeTransformation<TransformationState>(this, TransformationOptions.Disabled)

        member this.DeclarationLocations = this.SharedState.DeclarationLocations.ToLookup(fst, snd)

    let Accumulate (syntaxTree: IEnumerable<QsNamespace>) =
        let walker = SyntaxTreeTransformation()

        for ns in syntaxTree do
            walker.Namespaces.OnNamespace ns |> ignore

        walker.DeclarationLocations


type CodegenContext =
    {
        AssemblyConstants: IDictionary<string, string>
        AllQsElements: IEnumerable<QsNamespace>
        AllUdts: ImmutableDictionary<QsQualifiedName, QsCustomType>
        AllCallables: ImmutableDictionary<QsQualifiedName, QsCallable>
        DeclarationPositions: ImmutableDictionary<string, ImmutableSortedSet<Position>>
        ByName: ImmutableDictionary<string, (string * QsCallable) list>
        Current: QsQualifiedName option
        Signature: ResolvedSignature option
        FileName: string option
        EntryPoints: IEnumerable<QsQualifiedName>
    }
    static member public Create(syntaxTree, assemblyConstants) =
        let udts = GlobalTypeResolutions syntaxTree
        let callables = GlobalCallableResolutions syntaxTree
        let positionInfos = DeclarationLocations.Accumulate syntaxTree

        let callablesByName =
            let result = new Dictionary<string, (string * QsCallable) list>()

            syntaxTree
            |> Seq.collect (fun ns ->
                ns.Elements
                |> Seq.choose (function
                    | QsCallable c -> Some(ns, c)
                    | _ -> None))
            |> Seq.iter (fun (ns, c) ->
                if result.ContainsKey c.FullName.Name then
                    result.[c.FullName.Name] <- (ns.Name, c) :: result.[c.FullName.Name]
                else
                    result.[c.FullName.Name] <- [ ns.Name, c ])

            result.ToImmutableDictionary()

        {
            AssemblyConstants = assemblyConstants
            AllQsElements = syntaxTree
            ByName = callablesByName
            AllUdts = udts
            AllCallables = callables
            DeclarationPositions =
                positionInfos.ToImmutableDictionary((fun g -> g.Key), (fun g -> g.ToImmutableSortedSet()))
            Current = None
            FileName = None
            Signature = None
            EntryPoints = ImmutableArray.Empty
        }

    static member public Create(compilation: QsCompilation, assemblyConstants) =
        { CodegenContext.Create(compilation.Namespaces, assemblyConstants) with EntryPoints = compilation.EntryPoints }

    static member public Create(compilation: QsCompilation) =
        CodegenContext.Create(compilation, ImmutableDictionary.Empty)

    static member public Create(syntaxTree: ImmutableArray<QsNamespace>) =
        CodegenContext.Create(syntaxTree, ImmutableDictionary.Empty)

    member public this.ProcessorArchitecture =
        match this.AssemblyConstants.TryGetValue AssemblyConstants.ProcessorArchitecture with
        | true, name -> name
        | false, _ -> null

    member public this.ExecutionTarget =
        match this.AssemblyConstants.TryGetValue AssemblyConstants.ExecutionTarget with
        | true, name -> name
        | false, _ -> null

    member public this.AssemblyName =
        match this.AssemblyConstants.TryGetValue AssemblyConstants.AssemblyName with
        | true, name -> name
        | false, _ -> null

    member public this.ExposeReferencesViaTestNames =
        match this.AssemblyConstants.TryGetValue AssemblyConstants.ExposeReferencesViaTestNames with
        | true, propVal -> propVal = "true"
        | false, _ -> false

    member internal this.GenerateCodeForSource(fileName: string) =
        let targetsQuantumProcessor =
            match this.AssemblyConstants.TryGetValue AssemblyConstants.ProcessorArchitecture with
            | true, target ->
                target = AssemblyConstants.HoneywellProcessor
                || target = AssemblyConstants.IonQProcessor
                || target = AssemblyConstants.QCIProcessor
                || target = AssemblyConstants.QuantinuumProcessor
                || target = "MicrosoftSimulator" // ToDo: We need to have an assembly constant for this.
            | _ -> false

        not (fileName.EndsWith ".dll") || targetsQuantumProcessor
