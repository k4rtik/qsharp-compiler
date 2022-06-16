namespace Microsoft.Quantum.RoslynWrapper.Testing

open Xunit

open Microsoft.Quantum.RoslynWrapper

module NamespaceTests =

    [<Fact>]
    let ``namespace: empty`` () =
        let n = ``namespace`` "Foo" ``{`` [] [] ``}``
        let actual = toNamespaceCode n

        let expected =
            @"namespace Foo
{
}"

        areEqual expected actual

    [<Fact>]
    let ``namespace: with usings`` () =
        let n = ``namespace`` "Foo" ``{`` [ using "System.Collections" ] [] ``}``
        let actual = toNamespaceCode n

        let expected =
            @"namespace Foo
{
    using System.Collections;
}"

        areEqual expected actual

    [<Fact>]
    let ``namespace: with usings and classes`` () =
        let c = ``class`` "C" ``<<`` [] ``>>`` ``:`` None ``,`` [] [ ``public`` ] ``{`` [] ``}``
        let n = ``namespace`` "Foo" ``{`` [ using "System"; using "System.Collections" ] [ c ] ``}``
        let actual = toNamespaceCode n

        let expected =
            @"namespace Foo
{
    using System;
    using System.Collections;

    public class C
    {
    }
}"

        areEqual expected actual


    [<Fact>]
    let ``namespace: type aliases`` () =
        let n =
            ``namespace`` "Foo" ``{`` [ using "System"; using "System.Collections"; alias "Foo" "Int" ] [] ``}``

        let actual = toNamespaceCode n

        let expected =
            @"namespace Foo
{
    using System;
    using System.Collections;
    using Foo = Int;
}"

        areEqual expected actual
