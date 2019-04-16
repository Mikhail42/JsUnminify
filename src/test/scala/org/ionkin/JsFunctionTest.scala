package org.ionkin

import org.junit.Assert.{assertEquals, assertTrue}
import org.junit.Test

class JsFunctionTest {

  @Test
  def equals() {
    val js =
      """function {
        |   var b = 5;
        |   return "sds";
        |}""".stripMargin
    val ret = Return(Brace(js, """function """.stripMargin.length))
    assertEquals("""return "sds";;
                   |""".stripMargin, ret.unminifiedReturn)
    assertEquals("""
                   |   var b = 5;
                   |   return "sds";;
                   |""".stripMargin, ret.unminifiedBodyWithReturn())
  }

  @Test
  def simplest() {
    val js =
      """sdsd
        |function {
        |   var b = 5;
        |   const s = 5;
        |   return c = 5, n = 4, "sds"
        |}""".stripMargin
    val ret = Return(Brace(js, """sdsd
                                 |function """.stripMargin.length))
    assertEquals("""c = 5;
                   |   n = 4;
                   |   return "sds";
                   |""".stripMargin, ret.unminifiedReturn)
    assertEquals("""
                   |   var b = 5;
                   |   const s = 5;
                   |   c = 5;
                   |   n = 4;
                   |   return "sds";
                   |""".stripMargin, ret.unminifiedBodyWithReturn())
  }

  @Test
  def functionFindReturnTest() {
    val js =
      """{
        |   var b = 5;
        |   var g = {
        |     "hh": 9,
        |     k: 90
        |     f(): function() { return 5; }
        |   }
        |   const s = 5;
        |   return "sds"
        |}""".stripMargin
    val f = JsFunction(Brace(js, 0), None)
    val pos = f.offsetOfReturnInInner().get
    assertTrue(js.startsWith("return", pos + 1))
    assertEquals(js.length - """return "sds"
                               |}""".stripMargin.length - 1, f.offsetOfReturnInInner().get)
  }

  @Test
  def returnTestComplex() {
    val js =
      """{
        |   var b = 5;
        |   var g = {
        |     "hh": 9,
        |     k: 90
        |     f(): function() { return 5; }
        |   }
        |   const s = 5;
        |   return c = 5, (n = "return )}", a = "dfd", 5), "sds"
        |}""".stripMargin
    val ret = Return(Brace(js, 0))
    assertEquals("""c = 5;
                   |   (n = "return )}", a = "dfd", 5);
                   |   return "sds";
                   |""".stripMargin, ret.unminifiedReturn)
  }
/*
  @Test
  def fundInnerBodiesTest() {
    val js =
      """function a() {
        |   var b = 5;
        |   const s = 5;
        |   function sss1() {
        |     function sss5() {
        |       return 4 + 5
        |     }
        |     var a = 4;
        |     return a = 6, 4 + 5;
        |   }
        |   function sss2() {
        |     function sss3() {
        |       var a = 4;
        |       return a = 6, 4 + 5;
        |     }
        |     var a = 4;
        |     return a = 6, 4 + 5;
        |   }
        |   return c = 5, n = 4, "sds"
        |}
      """.stripMargin
    val inBody1 = """
                    |     function sss5() {
                    |       return 4 + 5
                    |     }
                    |     var a = 4;
                    |     return a = 6, 4 + 5;
                    |   """.stripMargin
    val inBody2 = """
                    |     function sss3() {
                    |       var a = 4;
                    |       return a = 6, 4 + 5;
                    |     }
                    |     var a = 4;
                    |     return a = 6, 4 + 5;
                    |   """.stripMargin
    val brace = Brace(js, js.indexOf("{"))
    val fun = JsFunction(brace)
    val rigth = List(inBody1, inBody2)
    assertEquals(rigth, fun.findInnerBodies().map(_.inner))
  }*/

  @Test
  def withInnerFunctionTest() {
    val js =
      """function a() {
        |   var b = 5;
        |   const s = 5;
        |   function sss1() {
        |     function sss5() {
        |       return 4 + 5
        |     }
        |     var a = 4;
        |     return a = 6, 4 + 5;
        |   }
        |   function sss2() {
        |     function sss3() {
        |       var a = 4;
        |       return a = 6, 4 + 5;
        |     }
        |     var a = 4;
        |     return a = 6, 4 + 5;
        |   }
        |   return c = 5, n = 4, "sds"
        |}
      """.stripMargin
    val fun = JsFunction(Brace(js, js.indexOf("{")))
    assertEquals(
      """
        |   var b = 5;
        |   const s = 5;
        |   function sss1() {
        |     function sss5() {
        |       return 4 + 5;
        |     }
        |     var a = 4;
        |     a = 6;
        |     return 4 + 5;;
        |   }
        |   function sss2() {
        |     function sss3() {
        |       var a = 4;
        |       a = 6;
        |       return 4 + 5;;
        |     }
        |     var a = 4;
        |     a = 6;
        |     return 4 + 5;;
        |   }
        |   c = 5;
        |   n = 4;
        |   return "sds";
      """.stripMargin, fun.unminifyBody())
  }
}
