namespace Microsoft.Quantum.RoslynWrapper.Testing

open Xunit

open Microsoft.Quantum.RoslynWrapper

module ConstructorTests =
    [<Fact>]
    let ``constructor: empty`` () =
        let m = constructor "C" ``(`` [] ``)`` ``:`` [] [ ``public`` ] ``{`` [] ``}``

        let actual = toClassMembersCode [ m ]

        let expected =
            @"namespace N
{
    using System;

    public class C
    {
        public C()
        {
        }
    }
}"

        areEqual expected actual

    [<Fact>]
    let ``constructor: with parameter`` () =
        let m =
            constructor "C" ``(`` [ ("thing", (``type`` "object")) ] ``)`` ``:`` [] [ ``public`` ] ``{`` [] ``}``

        let actual = toClassMembersCode [ m ]

        let expected =
            @"namespace N
{
    using System;

    public class C
    {
        public C(object thing)
        {
        }
    }
}"

        areEqual expected actual

    [<Fact>]
    let ``constructor: with parameter 2`` () =
        let m =
            constructor
                "C"
                ``(``
                [ ("thing", (``type`` "object")); ("name", (``type`` "string")) ]
                ``)``
                ``:``
                []
                [ ``public`` ]
                ``{``
                []
                ``}``

        let actual = toClassMembersCode [ m ]

        let expected =
            @"namespace N
{
    using System;

    public class C
    {
        public C(object thing, string name)
        {
        }
    }
}"

        areEqual expected actual

    [<Fact>]
    let ``constructor: calling base constructor`` () =
        let m =
            constructor
                "C"
                ``(``
                [ ("thing", (``type`` "object")) ]
                ``)``
                ``:``
                [ "thing" ]
                [ ``public`` ]
                ``{``
                []
                ``}``

        let actual = toClassMembersCode [ m ]

        let expected =
            @"namespace N
{
    using System;

    public class C
    {
        public C(object thing) : base(thing)
        {
        }
    }
}"

        areEqual expected actual

    [<Fact>]
    let ``constructor: calling base constructor 2`` () =
        let m =
            constructor
                "C"
                ``(``
                [ ("thing", (``type`` "object")); ("name", (``type`` "string")) ]
                ``)``
                ``:``
                [ "thing"; "name" ]
                [ ``public`` ]
                ``{``
                []
                ``}``

        let actual = toClassMembersCode [ m ]

        let expected =
            @"namespace N
{
    using System;

    public class C
    {
        public C(object thing, string name) : base(thing, name)
        {
        }
    }
}"

        areEqual expected actual

    [<Fact>]
    let ``constructor: private`` () =
        let m = constructor "C" ``(`` [] ``)`` ``:`` [] [ ``private`` ] ``{`` [] ``}``

        let actual = toClassMembersCode [ m ]

        let expected =
            @"namespace N
{
    using System;

    public class C
    {
        private C()
        {
        }
    }
}"

        areEqual expected actual
