package scalacss

object Css {

  def prepareStyles(styles: TraversableOnce[StyleA])(implicit env: Env): Stream[CssStyleEntry] =
    styles.toStream flatMap styleA

  def prepareKeyframes(styles: TraversableOnce[Keyframes])(implicit env: Env): Stream[CssKeyframesAnimation] =
    styles.toStream map keyframes

  def prepareFontFaces(styles: TraversableOnce[FontFace])(implicit env: Env): Stream[CssFontFace] =
    styles.toStream map fontFaces

  def className(cn: ClassName): CssSelector =
    "." + cn.value

  def selector(cn: ClassName, c: Cond): CssSelector =
    selector(className(cn), c)

  def selector(cs: CssSelector, c: Cond): CssSelector =
    c.pseudo.fold(cs)(_ modSelector cs)

  def mediaQuery(c: Cond): CssMediaQueryO =
    NonEmptyVector.option(c.mediaQueries) map Media.css

  def keyframes(frames: Keyframes)(implicit env: Env): CssKeyframesAnimation =
    CssKeyframesAnimation(frames.name, frames.frames.map(s => (s._1, styleA(s._2))).toMap)

  def fontFaces(ff: FontFace)(implicit env: Env): CssFontFace =
    CssFontFace(ff.fontFamily, ff.src, ff.fontStretch, ff.fontStyle, ff.fontWeight, ff.unicodeRange)

  def styleA(s: StyleA)(implicit env: Env): Stream[CssStyleEntry] =
    style(className(s.className), s.style)

  def style(sel: CssSelector, s: StyleS)(implicit env: Env): Stream[CssStyleEntry] = {
    def main: Stream[CssStyleEntry] =
      s.data.toStream.flatMap {
        case (cond, avs) =>
          val kvs = avs.avStream.map(_(env)).foldLeft(Vector.empty[CssKV])(_ ++ _)
          NonEmptyVector.maybe(kvs, Stream.empty[CssStyleEntry]) {c =>
            val mq = mediaQuery(cond)
            val s  = selector(sel, cond)
            Stream(CssStyleEntry(mq, s, c))
          }
        }

    def exts: Stream[CssStyleEntry] =
      s.unsafeExts.toStream.flatMap(unsafeExt(sel, _))

    main append exts
  }

  def unsafeExt(root: CssSelector, u: Style.UnsafeExt)(implicit env: Env): Stream[CssStyleEntry] = {
    val sel = u.sel(s"$root${u.cond}")
    style(sel, u.style)
  }

  type ValuesByMediaQuery = NonEmptyVector[(CssSelector, NonEmptyVector[CssKV])]
  type ByMediaQuery       = Map[CssMediaQueryO, ValuesByMediaQuery]

  def groupedByType(c: Css): (Stream[CssStyleEntry], Stream[CssKeyframesAnimation], Stream[CssFontFace]) = {
    val styles = Stream.newBuilder[CssStyleEntry]
    val animations = Stream.newBuilder[CssKeyframesAnimation]
    val fontFaces = Stream.newBuilder[CssFontFace]
    c.foreach {
      case e: CssStyleEntry => styles += e
      case e: CssKeyframesAnimation => animations += e
      case e: CssFontFace => fontFaces += e
    }
    (styles.result(), animations.result(), fontFaces.result())
  }

  def mapByMediaQuery(c: Stream[CssStyleEntry]): ByMediaQuery = {
    val z: ByMediaQuery = Map.empty
    c.foldLeft(z){(q, e) =>
      val add = (e.sel, e.content)
      val k = e.mq
      q.updated(k, NonEmptyVector.endO(q get k, add))
    }
  }

  def flatten(css: Stream[CssStyleEntry]): Stream[(CssMediaQueryO, CssSelector, CssKV)] =
    css.flatMap { case CssStyleEntry(mq, sel, kvs) =>
      kvs.toStream.map(kv => (mq, sel, kv))
    }

  type Flat4 = (CssMediaQueryO, CssSelector, String, String)
  def flatten4(css: Stream[CssStyleEntry]): Stream[Flat4] =
    css.flatMap { case CssStyleEntry(mq, sel, kvs) =>
      kvs.toStream.map(kv => (mq, sel, kv.key, kv.value))
    }
}