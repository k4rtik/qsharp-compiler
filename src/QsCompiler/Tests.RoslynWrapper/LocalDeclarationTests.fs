namespace Microsoft.Quantum.RoslynWrapper.Testing

open Xunit

open Microsoft.Quantum.RoslynWrapper

module LocalDeclarationTests =
    [<Fact>]
    let ``typed local variable: uninitialized`` () =
        let s = ``typed var`` "string" "name" None
        let m = hostInMethod "void" [ s ]
        let actual = toClassMembersCode [ m ]

        let expected =
            @"namespace N
{
    using System;

    public class C
    {
        protected internal void Host()
        {
            string name;
        }
    }
}"

        areEqual expected actual

    [<Fact>]
    let ``typed local variable: initialized`` () =
        let e = ``:=`` <| literal "John"
        let s = ``typed var`` "string" "name" <| Some e
        let m = hostInMethod "void" [ s ]
        let actual = toClassMembersCode [ m ]

        let expected =
            @"namespace N
{
    using System;

    public class C
    {
        protected internal void Host()
        {
            string name = ""John"";
        }
    }
}"

        areEqual expected actual

    [<Fact>]
    let ``untyped local variable: initialized`` () =
        let s = var "name" (``:=`` <| ``default`` "String")
        let m = hostInMethod "void" [ s ]
        let actual = toClassMembersCode [ m ]

        let expected =
            @"namespace N
{
    using System;

    public class C
    {
        protected internal void Host()
        {
            var name = default(String);
        }
    }
}"

        areEqual expected actual
