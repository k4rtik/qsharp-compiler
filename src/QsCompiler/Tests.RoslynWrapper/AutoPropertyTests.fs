namespace Microsoft.Quantum.RoslynWrapper.Testing

open Xunit

open Microsoft.Quantum.RoslynWrapper

module AutoPropertyTests =

    [<Fact>]
    let ``property: read`` () =
        let m = ``property-get`` "string" "Name" [ ``public`` ] get [ ``return`` (Some(literal "")) ]

        let actual = toClassMembersCode [ m ]

        let expected =
            @"namespace N
{
    using System;

    public class C
    {
        public string Name
        {
            get
            {
                return """";
            }
        }
    }
}"

        areEqual expected actual


    [<Fact>]
    let ``property: read -arrow-`` () =
        let bodyBlock = [ ``return`` (Some(literal "")) ] |> Microsoft.CodeAnalysis.CSharp.SyntaxFactory.Block

        let m = ``property-arrow_get`` "string" "Name" [ ``public`` ] get (``=>`` (ident "s")) // (``() =>`` [] bodyBlock))

        let actual = toClassMembersCode [ m ]

        let expected =
            @"namespace N
{
    using System;

    public class C
    {
        public string Name => s;
    }
}"

        areEqual expected actual


    [<Fact>]
    let ``property: readwrite`` () =
        let m =
            property
                "string"
                "Name"
                [ ``public`` ]
                get
                [ ``return`` (Some(literal "hardcoded")) ]
                set
                [ statement ((ident "test") <-- (ident "value")) ]

        let actual = toClassMembersCode [ m ]

        let expected =
            @"namespace N
{
    using System;

    public class C
    {
        public string Name
        {
            get
            {
                return ""hardcoded"";
            }

            set
            {
                test = value;
            }
        }
    }
}"

        areEqual expected actual

    [<Fact>]
    let ``auto property: read-only`` () =
        let m = propg "string" "Name" [ ``public`` ]

        let actual = toClassMembersCode [ m ]

        let expected =
            @"namespace N
{
    using System;

    public class C
    {
        public string Name
        {
            get;
        }
    }
}"

        areEqual expected actual

    [<Fact>]
    let ``auto property: read-write`` () =
        let m = prop "string" "Name" [ ``public`` ]

        let actual = toClassMembersCode [ m ]

        let expected =
            @"namespace N
{
    using System;

    public class C
    {
        public string Name
        {
            get;
            set;
        }
    }
}"

        areEqual expected actual
