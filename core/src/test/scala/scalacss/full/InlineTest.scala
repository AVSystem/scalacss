package scalacss.full

import scala.concurrent.duration._
import scalacss.Defaults._
import scalacss.UnicodeRange

//object CopyDefaultsForInline extends Defaults
//import CopyDefaultsForInline._

object MyInline extends StyleSheet.Inline {
  import dsl._

  val noMacrosOrClassnameHintHere =
    style("manual")(
      margin(12 px),
      padding(0.5 ex),
      cursor.pointer,
      textDecorationLine.underline.overline,
      backgroundImage := "radial-gradient(5em circle at top left, yellow, blue)",

      &.hover(
        fontWeight.normal,
        lineHeight(1 em),
        padding.`0`,
        cursor.zoomIn
      ),

      &.visited.not(_.firstChild)(
        animationDelay(1 minute, 50 millis),
        fontWeight.bold,
        font := ^.inherit
      ),

      (media.tv.minDeviceAspectRatio(4 :/: 3) & media.all.resolution(300 dpi))(
        margin.vertical(10 em)
      ),

      media.not.handheld.landscape.color - (
        padding.horizontal(500 px)
      ),

      unsafeChild("nav.debug")(
        backgroundColor(Color("#f88")), // Bypass validation
        color.black.important,

        unsafeChild("h1")(
          lineHeight(97.5 %%),
          fontSize(150 %%)
        )
      ),

      unsafeRoot("blockquote:before, blockquote:after")(
        content := "''",
        content := none
      ),
      unsafeRoot(".DEBUG")(
        borderColor(c"#080")
      )
    )

  val `it's a mixin!` = mixin(color.brown)

  val medianess = style(
    media.maxWidth(100 px)(
      // https://github.com/japgolly/scalacss/issues/41
      unsafeChild("span")(backgroundColor(yellow)),
      // https://github.com/japgolly/scalacss/issues/38
      `it's a mixin!`)
  )

  val condMixins = mixin(
    display.block,
    media.maxWidth(100 px)(margin.auto),
    &.hover(color.red))

  val condMixinP = style(&.hover(condMixins))
  val condMixinQ = style(media.maxWidth(100 px)(condMixins))

  val empty = style(null: String)()

  /** Composite style */
  val sc = styleC {
    val o = styleS(border(1 px, solid, black), padding(1 ex))
    val l = styleS(fontWeight.bold)
    val c = styleS(margin(4 ex), backgroundColor(c"#eee"))
    o.named('outer) :*: l.named('label) :*: c.named('checkbox)
  }
}

object MyInline2 extends StyleSheet.Inline {
  import dsl._

  /** Style applying Bootstrap */
  val sb1 = style(addClassNames("btn", "btn-default"))

  /** Style extending into Bootstrap */
  val sb2 = style(
    addClassNames("btn", "btn-default"),
    marginTop.inherit
  )

  /** Style requiring boolean */
  val everythingOk =
    styleF.bool(ok => styleS(
      backgroundColor(if (ok) green else red),
      maxWidth(80.ex)
    ))

  /** Style requiring int */
  val indent: Int => StyleA =
    styleF.int(2 to 4)(i => styleS(
      paddingLeft(i * 4.ex),
      mixinIf(i == 3)(color.red),
      mixinIfElse(i == 2)(color.blue)(marginTop(1 em))
    ))

  /** Other styleF */
  val opbool =
    styleF(Domain.boolean.option) {
      case None        => styleS(color.black)
      case Some(false) => styleS(color.red)
      case Some(true)  => styleS(color.green)
    }

  val `what the hell??` = style(visibility.hidden)
}

object MyInline3 extends StyleSheet.Inline {
  import dsl._

  val dup1a = style("MyInline3-dup1b")(wordBreak.breakAll)
  val dup1b = style(wordBreak.keepAll)

  val dup2a = style("MyInline3-dup2c")(verticalAlign.top)
  val dup2b = style("MyInline3-dup2c-2")(verticalAlign.middle)
  val dup2c = style(verticalAlign.bottom)

  object innerObject {

    val mybool =
      styleF("blah").bool(ok => styleS(
        backgroundColor(if (ok) green else red),
        maxWidth(80.ex)
      ))

    val depth1 = style(borderCollapse.collapse)
    object andAgain {
      val depth2 = style(borderCollapse.separate)
    }
  }

  initInnerObjects(
    innerObject.depth1,
    innerObject.andAgain.depth2)
}

object MyInlineWithKeyframes extends StyleSheet.Inline {
  import dsl._

  val s = style(
    height(100 px),
    width(30 px)
  )

  val kf1 = keyframes(
    (0 %%) -> s,
    (20 %%) -> kstyle(
      height(150 px),
      width(30 px)
    ),
    (100 %%) -> kstyle(
      height(200 px),
      width(60 px)
    )
  )
}

object MyInlineWithFontFace extends StyleSheet.Inline {
  val ff = fontFace("myFont", "url(font.woff)", "expanded", "italic", "bold", UnicodeRange(0, 5))
  val ff2 = fontFace("myFont2", "url(font2.woff)", "expanded", "italic", "200", UnicodeRange(0, 1114111))
  val ff3 = fontFace("myFont3", Seq("local(HelveticaNeue)", "url(font2.woff)"), "expanded", "italic", "200", UnicodeRange(0, 1114111))
}

object MyInlineComplexCond extends StyleSheet.Inline {
  import dsl._

  val cond =
    style("manual")(
      margin(12 px),
      padding(0.5 ex),

      &.hover(
        cursor.zoomIn,
        media.maxWidth(150 px)(
          margin(0 px)
        ),

        unsafeChild(".child")(
          media.maxWidth(100 px)(
            margin(2 px)
          ),
          margin(5 px)
        )
      ),

      unsafeChild(".child2")(
        &.hover(
          margin(15 px),
          media.maxWidth(100 px)(
            margin(10 px)
          )
        )
      ),

      &.nthChild(5)(
        cursor.zoomIn,
        media.maxWidth(150 px)(
          margin(0 px)
        ),

        unsafeChild(".row")(
          media.maxWidth(100 px)(
            margin(2 px)
          ),
          margin(5 px)
        )
      ),

      unsafeChild(".row2")(
        &.nthChild(15)(
          margin(15 px),
          media.maxWidth(100 px)(
            margin(10 px)
          )
        )
      ),

      &.attr("some-attribute", "true") (
        &.after (
          padding(5 px)
        )
      )
    )
}

object InlineTest extends utest.TestSuite {
  import utest._

  import scalacss.TestUtil._

  def norm(css: String) = css.trim

  override def tests = TestSuite {
    'css1 - assertEq(norm(MyInline.render), norm(
      """
        |.manual:not(:first-child):visited {
        |  -o-animation-delay: 60s,50ms;
        |  -webkit-animation-delay: 60s,50ms;
        |  -moz-animation-delay: 60s,50ms;
        |  animation-delay: 60s,50ms;
        |  font-weight: bold;
        |  font: inherit;
        |}
        |
        |.manual:hover {
        |  font-weight: normal;
        |  line-height: 1em;
        |  padding: 0;
        |  cursor: -webkit-zoom-in;
        |  cursor: -moz-zoom-in;
        |  cursor: -o-zoom-in;
        |  cursor: zoom-in;
        |}
        |
        |.manual {
        |  margin: 12px;
        |  padding: 0.5ex;
        |  cursor: pointer;
        |  -webkit-text-decoration-line: underline overline;
        |  -moz-text-decoration-line: underline overline;
        |  text-decoration-line: underline overline;
        |  background-image: -o-radial-gradient(5em circle at top left, yellow, blue);
        |  background-image: -webkit-radial-gradient(5em circle at top left, yellow, blue);
        |  background-image: -moz-radial-gradient(5em circle at top left, yellow, blue);
        |  background-image: radial-gradient(5em circle at top left, yellow, blue);
        |}
        |
        |.manual nav.debug {
        |  background-color: #f88;
        |  color: black !important;
        |}
        |
        |.manual nav.debug h1 {
        |  line-height: 97.5%;
        |  font-size: 150%;
        |}
        |
        |blockquote:before, blockquote:after {
        |  content: '';
        |  content: none;
        |}
        |
        |.DEBUG {
        |  border-color: #080;
        |}
        |
        |.MyInline-condMixinP:hover {
        |  display: block;
        |  color: red;
        |}
        |
        |.MyInline-0002 {
        |  border: 1px solid black;
        |  padding: 1ex;
        |}
        |
        |.MyInline-0003 {
        |  font-weight: bold;
        |}
        |
        |.MyInline-0004 {
        |  margin: 4ex;
        |  background-color: #eee;
        |}
        |
        |@media not handheld and (orientation:landscape) and (color) {
        |  .manual {
        |    padding-left: 500px;
        |    padding-right: 500px;
        |  }
        |}
        |
        |@media tv and (min-device-aspect-ratio:3/4), all and (resolution:300dpi) {
        |  .manual {
        |    margin-top: 10em;
        |    margin-bottom: 10em;
        |  }
        |}
        |
        |@media (max-width:100px) {
        |  .MyInline-medianess {
        |    color: brown;
        |  }
        |  .MyInline-medianess span {
        |    background-color: yellow;
        |  }
        |  .MyInline-condMixinP:hover {
        |    margin: auto;
        |  }
        |  .MyInline-condMixinQ {
        |    display: block;
        |    margin: auto;
        |  }
        |  .MyInline-condMixinQ:hover {
        |    color: red;
        |  }
        |}
      """.stripMargin))

    'css2 - assertEq(norm(MyInline2.render), norm(
      """
        |.MyInline2-sb2 {
        |  margin-top: inherit;
        |}
        |
        |.MyInline2-everythingOk-t {
        |  background-color: green;
        |  max-width: 80ex;
        |}
        |
        |.MyInline2-everythingOk-f {
        |  background-color: red;
        |  max-width: 80ex;
        |}
        |
        |.MyInline2-indent-2 {
        |  padding-left: 8ex;
        |  color: blue;
        |}
        |
        |.MyInline2-indent-3 {
        |  padding-left: 12ex;
        |  color: red;
        |  margin-top: 1em;
        |}
        |
        |.MyInline2-indent-4 {
        |  padding-left: 16ex;
        |  margin-top: 1em;
        |}
        |
        |.MyInline2-opbool-1 {
        |  color: black;
        |}
        |
        |.MyInline2-opbool-2 {
        |  color: green;
        |}
        |
        |.MyInline2-opbool-3 {
        |  color: red;
        |}
        |
        |.MyInline2-what_the_hell__ {
        |  visibility: hidden;
        |}
      """.stripMargin))

    'css3 - assertEq(norm(MyInline3.render), norm(
      """
        |.MyInline3-dup1b {
        |  word-break: break-all;
        |}
        |
        |.MyInline3-dup1b-2 {
        |  word-break: keep-all;
        |}
        |
        |.MyInline3-dup2c {
        |  vertical-align: top;
        |}
        |
        |.MyInline3-dup2c-2 {
        |  vertical-align: middle;
        |}
        |
        |.MyInline3-dup2c-3 {
        |  vertical-align: bottom;
        |}
        |
        |.blah-t {
        |  background-color: green;
        |  max-width: 80ex;
        |}
        |
        |.blah-f {
        |  background-color: red;
        |  max-width: 80ex;
        |}
        |
        |.MyInline3-innerObject-depth1 {
        |  border-collapse: collapse;
        |}
        |
        |.MyInline3-innerObject-andAgain-depth2 {
        |  border-collapse: separate;
        |}
      """.stripMargin))

    'classnames {
      'manual - assertEq(MyInline.noMacrosOrClassnameHintHere.htmlClass, "manual")

      'manualF - {
        assertEq(MyInline3.innerObject.mybool(true).htmlClass, "blah-t")
        assertEq(MyInline3.innerObject.mybool(false).htmlClass, "blah-f")
      }

      'everythingOk {
        assertEq(MyInline2.everythingOk(true) .htmlClass, "MyInline2-everythingOk-t")
        assertEq(MyInline2.everythingOk(false).htmlClass, "MyInline2-everythingOk-f")
      }

      'indent {
        assertEq(MyInline2.indent(2).htmlClass, "MyInline2-indent-2")
        assertEq(MyInline2.indent(3).htmlClass, "MyInline2-indent-3")
        assertEq(MyInline2.indent(4).htmlClass, "MyInline2-indent-4")
      }

      'opbool {
        assertEq(MyInline2.opbool(None)       .htmlClass, "MyInline2-opbool-1")
        assertEq(MyInline2.opbool(Some(true)) .htmlClass, "MyInline2-opbool-2")
        assertEq(MyInline2.opbool(Some(false)).htmlClass, "MyInline2-opbool-3")
      }

      'sb1 - assertEq(MyInline2.sb1.htmlClass, "btn btn-default")
      'sb2 - assertEq(MyInline2.sb2.htmlClass, "MyInline2-sb2 btn btn-default")

      'empty - assertEq(MyInline.empty.htmlClass, "MyInline-0001")

      'wth - assertEq(MyInline2.`what the hell??`.htmlClass, "MyInline2-what_the_hell__")

      'dup1 - {
        assertEq(MyInline3.dup1a.htmlClass, "MyInline3-dup1b")
        assertEq(MyInline3.dup1b.htmlClass, "MyInline3-dup1b-2")
      }

      'dup2 - {
        assertEq(MyInline3.dup2a.htmlClass, "MyInline3-dup2c")
        assertEq(MyInline3.dup2b.htmlClass, "MyInline3-dup2c-2")
        assertEq(MyInline3.dup2c.htmlClass, "MyInline3-dup2c-3")
      }

      'innerObject_1 - assertEq(MyInline3.innerObject.depth1.htmlClass, "MyInline3-innerObject-depth1")
      'innerObject_2 - assertEq(MyInline3.innerObject.andAgain.depth2.htmlClass, "MyInline3-innerObject-andAgain-depth2")

      'styleC {
        import shapeless.syntax.singleton._
        val classNames =
          MyInline.sc('outer)(o =>
                      _('label)(l =>
                        _('checkbox)(c =>
                          List(o, l, c).map(_.htmlClass))))
        assertEq(classNames, List("MyInline-0002", "MyInline-0003", "MyInline-0004"))
      }
    }

    'keyframes - assertEq(norm(MyInlineWithKeyframes.render), norm("""
       |@keyframes MyInlineWithKeyframes-kf1 {
       |  0% {
       |  height: 100px;
       |  width: 30px;
       |  }
       |
       |  20% {
       |  height: 150px;
       |  width: 30px;
       |  }
       |
       |  100% {
       |  height: 200px;
       |  width: 60px;
       |  }
       |
       |}
       |
       |.MyInlineWithKeyframes-s {
       |  height: 100px;
       |  width: 30px;
       |}
     """.stripMargin))

    'fontFaces - assertEq(norm(MyInlineWithFontFace.render), norm("""
         |@font-face {
         |  font-family: "myFont";
         |  src: url(font.woff);
         |  font-stretch: expanded;
         |  font-style: italic;
         |  font-weight: bold;
         |  unicode-range: U+0-5;
         |}
         |
         |@font-face {
         |  font-family: "myFont2";
         |  src: url(font2.woff);
         |  font-stretch: expanded;
         |  font-style: italic;
         |  font-weight: 200;
         |  unicode-range: U+0-10ffff;
         |}
         |
         |@font-face {
         |  font-family: "myFont3";
         |  src: local(HelveticaNeue), url(font2.woff);
         |  font-stretch: expanded;
         |  font-style: italic;
         |  font-weight: 200;
         |  unicode-range: U+0-10ffff;
         |}
       """.stripMargin))

    'complexCond - assertEq(norm(MyInlineComplexCond.render), norm(
      """.manual[some-attribute="true"]::after {
        |  padding: 5px;
        |}
        |
        |.manual:hover {
        |  cursor: -webkit-zoom-in;
        |  cursor: -moz-zoom-in;
        |  cursor: -o-zoom-in;
        |  cursor: zoom-in;
        |}
        |
        |.manual:nth-child(5) {
        |  cursor: -webkit-zoom-in;
        |  cursor: -moz-zoom-in;
        |  cursor: -o-zoom-in;
        |  cursor: zoom-in;
        |}
        |
        |.manual {
        |  margin: 12px;
        |  padding: 0.5ex;
        |}
        |
        |.manual:hover .child {
        |  margin: 5px;
        |}
        |
        |.manual .child2:hover {
        |  margin: 15px;
        |}
        |
        |.manual:nth-child(5) .row {
        |  margin: 5px;
        |}
        |
        |.manual .row2:nth-child(15) {
        |  margin: 15px;
        |}
        |
        |@media (max-width:150px) {
        |  .manual:nth-child(5) {
        |    margin: 0;
        |  }
        |  .manual:hover {
        |    margin: 0;
        |  }
        |}
        |
        |@media (max-width:100px) {
        |  .manual:hover .child {
        |    margin: 2px;
        |  }
        |  .manual .child2:hover {
        |    margin: 10px;
        |  }
        |  .manual:nth-child(5) .row {
        |    margin: 2px;
        |  }
        |  .manual .row2:nth-child(15) {
        |    margin: 10px;
        |  }
        |}
      """.stripMargin))
  }
}
