namespace Microsoft.Quantum.RoslynWrapper.Testing

open Xunit

open Microsoft.Quantum.RoslynWrapper

module InterfaceTests =
    [<Fact>]
    let ``interface: empty`` () =
        let i = ``interface`` "I" ``<<`` [] ``>>`` ``:`` [] [ ``public`` ] ``{`` [] ``}``
        let actual = toNamespaceMemberCode i

        let expected =
            @"namespace N
{
    using System;

    public interface I
    {
    }
}"

        areEqual expected actual

    [<Fact>]
    let ``interface: generic`` () =
        let i = ``interface`` "I" ``<<`` [ "T" ] ``>>`` ``:`` [] [ ``public`` ] ``{`` [] ``}``
        let actual = toNamespaceMemberCode i

        let expected =
            @"namespace N
{
    using System;

    public interface I<T>
    {
    }
}"

        areEqual expected actual

    [<Fact>]
    let ``interface: generic 2`` () =
        let i = ``interface`` "I" ``<<`` [ "R"; "S" ] ``>>`` ``:`` [] [ ``public`` ] ``{`` [] ``}``
        let actual = toNamespaceMemberCode i

        let expected =
            @"namespace N
{
    using System;

    public interface I<R, S>
    {
    }
}"

        areEqual expected actual

    [<Fact>]
    let ``interface: base interfaces`` () =
        let i =
            ``interface`` "I" ``<<`` [] ``>>`` ``:`` [ "IEnumerable"; "ISerializable" ] [ ``public`` ] ``{`` [] ``}``

        let actual = toNamespaceMemberCode i

        let expected =
            @"namespace N
{
    using System;

    public interface I : IEnumerable, ISerializable
    {
    }
}"

        areEqual expected actual

    [<Fact>]
    let ``interface: private`` () =
        let i = ``interface`` "I" ``<<`` [] ``>>`` ``:`` [] [ ``private`` ] ``{`` [] ``}``
        let actual = toNamespaceMemberCode i

        let expected =
            @"namespace N
{
    using System;

    private interface I
    {
    }
}"

        areEqual expected actual

    [<Fact>]
    let ``interface: static`` () =
        let i = ``interface`` "I" ``<<`` [] ``>>`` ``:`` [] [ ``static`` ] ``{`` [] ``}``
        let actual = toNamespaceMemberCode i

        let expected =
            @"namespace N
{
    using System;

    static interface I
    {
    }
}"

        areEqual expected actual

    [<Fact>]
    let ``interface: internal`` () =
        let i = ``interface`` "I" ``<<`` [] ``>>`` ``:`` [] [ ``internal`` ] ``{`` [] ``}``
        let actual = toNamespaceMemberCode i

        let expected =
            @"namespace N
{
    using System;

    internal interface I
    {
    }
}"

        areEqual expected actual

    [<Fact>]
    let ``interface: partial`` () =
        let i = ``interface`` "I" ``<<`` [] ``>>`` ``:`` [] [ partial ] ``{`` [] ``}``
        let actual = toNamespaceMemberCode i

        let expected =
            @"namespace N
{
    using System;

    partial interface I
    {
    }
}"

        areEqual expected actual

    [<Fact>]
    let ``interface: private static partial`` () =
        let i = ``interface`` "I" ``<<`` [] ``>>`` ``:`` [] [ ``private``; ``static``; partial ] ``{`` [] ``}``
        let actual = toNamespaceMemberCode i

        let expected =
            @"namespace N
{
    using System;

    private static partial interface I
    {
    }
}"

        areEqual expected actual
