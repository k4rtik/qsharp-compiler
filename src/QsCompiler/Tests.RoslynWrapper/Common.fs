namespace Microsoft.Quantum.RoslynWrapper.Testing

#nowarn "46" // Backticks removed by Fantomas: https://github.com/fsprojects/fantomas/issues/2034

open Xunit
open System.Text.RegularExpressions

open Microsoft.Quantum.RoslynWrapper

[<AutoOpen>]
module internal Common =
    let private normalizeNewLine s = Regex.Replace(s, "(?<!\r)\n", "\r\n")

    let areEqual (expected: string) (actual: string) =
        (expected, actual) |> (mapTuple2 normalizeNewLine >> Assert.Equal)

    let toNamespaceCode n =
        let cu = ``compilation unit`` [] [] [ n ]
        generateCodeToString cu

    let toNamespaceMemberCode c =
        let ns = ``namespace`` "N" ``{`` [ using "System" ] [ c ] ``}``
        toNamespaceCode ns

    let toClassMembersCode ms =
        let c = ``class`` "C" ``<<`` [] ``>>`` ``:`` None ``,`` [] [ ``public`` ] ``{`` ms ``}``
        toNamespaceMemberCode c

    let toInterfaceMembersCode ms =
        let c = ``interface`` "I" ``<<`` [] ``>>`` ``:`` [] [ ``public`` ] ``{`` ms ``}``
        toNamespaceMemberCode c

    let hostInMethod t ss =
        method t "Host" ``<<`` [] ``>>`` ``(`` [] ``)`` [ protected; ``internal`` ] ``{`` ss ``}``

    let returnFromArrowMethod t s =
        arrow_method t "Host" ``<<`` [] ``>>`` ``(`` [] ``)`` [ protected; ``internal`` ] (Some <| ``=>`` s)
