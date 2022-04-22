﻿// Copyright (c) Microsoft Corporation.
// Licensed under the MIT License.

namespace Microsoft.Quantum.QsCompiler.SyntaxProcessing.CapabilityInference

open Microsoft.Quantum.QsCompiler
open Microsoft.Quantum.QsCompiler.DataTypes
open Microsoft.Quantum.QsCompiler.DependencyAnalysis
open Microsoft.Quantum.QsCompiler.SymbolManagement
open Microsoft.Quantum.QsCompiler.SyntaxTree
open Microsoft.Quantum.QsCompiler.Transformations.Core

type Target = { Capability: RuntimeCapability; Architecture: string }

type IPattern =
    abstract Capability: RuntimeCapability

    abstract Diagnose: target: Target -> QsCompilerDiagnostic option

    abstract Explain: target: Target * nsManager: NamespaceManager * graph: CallGraph -> QsCompilerDiagnostic seq

type internal AnalyzerEnvironment = { CallableKind: QsCallableKind }

type internal AnalyzerAction = SyntaxTreeTransformation -> unit

type internal Analyzer = AnalyzerEnvironment -> AnalyzerAction -> IPattern seq

type LocationTrackingTransformation(options) =
    inherit SyntaxTreeTransformation(options)

    let mutable absolute = Null
    let mutable relative = Null

    member _.Offset =
        match absolute, relative with
        | Value a, Value r -> a + r |> Value
        | Value a, Null -> Value a
        | Null, _ -> Null

    override _.OnAbsoluteLocation location =
        absolute <- location |> QsNullable<_>.Map (fun l -> l.Offset)
        relative <- Null
        location

    override _.OnRelativeLocation location =
        relative <- location |> QsNullable<_>.Map (fun l -> l.Offset)
        location
