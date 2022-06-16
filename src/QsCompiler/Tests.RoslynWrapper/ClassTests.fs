namespace Microsoft.Quantum.RoslynWrapper.Testing

open Xunit

open Microsoft.Quantum.RoslynWrapper

module ClassTests =
    [<Fact>]
    let ``class: empty`` () =
        let c = ``class`` "C" ``<<`` [] ``>>`` ``:`` None ``,`` [] [ ``public`` ] ``{`` [] ``}``
        let actual = toNamespaceMemberCode c

        let expected =
            @"namespace N
{
    using System;

    public class C
    {
    }
}"

        areEqual expected actual

    [<Fact>]
    let ``class: base class`` () =
        let c =
            ``class`` "C" ``<<`` [] ``>>`` ``:`` (Some("B" |> simpleBase)) ``,`` [] [ ``public`` ] ``{`` [] ``}``

        let actual = toNamespaceMemberCode c

        let expected =
            @"namespace N
{
    using System;

    public class C : B
    {
    }
}"

        areEqual expected actual


    [<Fact>]
    let ``class: generic`` () =
        let c = ``class`` "C" ``<<`` [ "T" ] ``>>`` ``:`` None ``,`` [] [ ``public`` ] ``{`` [] ``}``
        let actual = toNamespaceMemberCode c

        let expected =
            @"namespace N
{
    using System;

    public class C<T>
    {
    }
}"

        areEqual expected actual

    [<Fact>]
    let ``class: generic 2`` () =
        let c = ``class`` "C" ``<<`` [ "R"; "S" ] ``>>`` ``:`` None ``,`` [] [ ``public`` ] ``{`` [] ``}``
        let actual = toNamespaceMemberCode c

        let expected =
            @"namespace N
{
    using System;

    public class C<R, S>
    {
    }
}"

        areEqual expected actual

    [<Fact>]
    let ``class: interfaces`` () =
        let c =
            ``class``
                "C"
                ``<<``
                []
                ``>>``
                ``:``
                None
                ``,``
                [ "IEnumerable" |> simpleBase; "ISerializable" |> simpleBase ]
                [ ``public`` ]
                ``{``
                []
                ``}``

        let actual = toNamespaceMemberCode c

        let expected =
            @"namespace N
{
    using System;

    public class C : IEnumerable, ISerializable
    {
    }
}"

        areEqual expected actual

    [<Fact>]
    let ``class: base and interfaces`` () =
        let c =
            ``class``
                "C"
                ``<<``
                []
                ``>>``
                ``:``
                (Some("B" |> simpleBase))
                ``,``
                [ "IEnumerable" |> simpleBase; "ISerializable" |> simpleBase ]
                [ ``public`` ]
                ``{``
                []
                ``}``

        let actual = toNamespaceMemberCode c

        let expected =
            @"namespace N
{
    using System;

    public class C : B, IEnumerable, ISerializable
    {
    }
}"

        areEqual expected actual

    [<Fact>]
    let ``class: private`` () =
        let c = ``class`` "C" ``<<`` [] ``>>`` ``:`` None ``,`` [] [ ``private`` ] ``{`` [] ``}``
        let actual = toNamespaceMemberCode c

        let expected =
            @"namespace N
{
    using System;

    private class C
    {
    }
}"

        areEqual expected actual

    [<Fact>]
    let ``class: static`` () =
        let c = ``class`` "C" ``<<`` [] ``>>`` ``:`` None ``,`` [] [ ``static`` ] ``{`` [] ``}``
        let actual = toNamespaceMemberCode c

        let expected =
            @"namespace N
{
    using System;

    static class C
    {
    }
}"

        areEqual expected actual

    [<Fact>]
    let ``class: internal`` () =
        let c = ``class`` "C" ``<<`` [] ``>>`` ``:`` None ``,`` [] [ ``internal`` ] ``{`` [] ``}``
        let actual = toNamespaceMemberCode c

        let expected =
            @"namespace N
{
    using System;

    internal class C
    {
    }
}"

        areEqual expected actual

    [<Fact>]
    let ``class: partial`` () =
        let c = ``class`` "C" ``<<`` [] ``>>`` ``:`` None ``,`` [] [ partial ] ``{`` [] ``}``
        let actual = toNamespaceMemberCode c

        let expected =
            @"namespace N
{
    using System;

    partial class C
    {
    }
}"

        areEqual expected actual

    [<Fact>]
    let ``class: private static partial`` () =
        let c =
            ``class`` "C" ``<<`` [] ``>>`` ``:`` None ``,`` [] [ ``private``; ``static``; partial ] ``{`` [] ``}``

        let actual = toNamespaceMemberCode c

        let expected =
            @"namespace N
{
    using System;

    private static partial class C
    {
    }
}"

        areEqual expected actual
