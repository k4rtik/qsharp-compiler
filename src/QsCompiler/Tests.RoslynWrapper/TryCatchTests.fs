namespace Microsoft.Quantum.RoslynWrapper.Testing

open Xunit

open Microsoft.Quantum.RoslynWrapper

module TryCatchTests =
    [<Fact>]
    let ``expression: try catch`` () =
        let t = ``try`` [ statement (ident "a" <-- literal 42) ] [ catch (Some("Exception", "e")) [] ] None
        let m = hostInMethod "void" [ t ]
        let actual = toClassMembersCode [ m ]

        let expected =
            @"namespace N
{
    using System;

    public class C
    {
        protected internal void Host()
        {
            try
            {
                a = 42;
            }
            catch (Exception e)
            {
            }
        }
    }
}"

        areEqual expected actual


    [<Fact>]
    let ``expression: try finally`` () =
        let a = var "a" (``:=`` <| literal "")

        let t =
            ``try``
                [ statement (ident "a" <-- literal 42) ]
                []
                (Some(``finally`` [ statement (invoke (ident "Apply") ``(`` [ ident "a" ] ``)``) ]))

        let r = ``return`` (Some(ident "a"))
        let m = hostInMethod "string" [ a; t; r ]
        let actual = toClassMembersCode [ m ]

        let expected =
            @"namespace N
{
    using System;

    public class C
    {
        protected internal string Host()
        {
            var a = """";
            try
            {
                a = 42;
            }
            finally
            {
                Apply(a);
            }

            return a;
        }
    }
}"

        areEqual expected actual

    [<Fact>]
    let ``expression: try catch finally`` () =
        let a = var "a" (``:=`` <| literal "")

        let t =
            ``try``
                [ statement (ident "a" <-- literal "foo") ]
                [
                    catch (Some("Exception1", "e1")) [ statement (ident "a" <-- literal "bar") ]
                    catch (Some("Exception2", "e2")) []
                    catch None [ statement (ident "a" <-- literal "other") ]

                ]
                (Some(``finally`` [ statement (invoke (ident "Apply") ``(`` [ ident "a" ] ``)``) ]))

        let m = hostInMethod "void" [ a; t ]
        let actual = toClassMembersCode [ m ]

        let expected =
            @"namespace N
{
    using System;

    public class C
    {
        protected internal void Host()
        {
            var a = """";
            try
            {
                a = ""foo"";
            }
            catch (Exception1 e1)
            {
                a = ""bar"";
            }
            catch (Exception2 e2)
            {
            }
            catch
            {
                a = ""other"";
            }
            finally
            {
                Apply(a);
            }
        }
    }
}"

        areEqual expected actual
