package japgolly.scalacss

import shapeless.Witness

object Values {

  final val w0   = Witness(0)
  final val w100 = Witness(100)
  final val w200 = Witness(200)
  final val w300 = Witness(300)
  final val w400 = Witness(400)
  final val w500 = Witness(500)
  final val w600 = Witness(600)
  final val w700 = Witness(700)
  final val w800 = Witness(800)
  final val w900 = Witness(900)

  final case class Str(value: String)

  implicit def cssValueFromStr(s: Str): Value = s.value
  implicit def cssValueFromInt(w: Witness.Lt[Int]): Value = {val i: Int = w.value; i.toString}

  final val absolute              = Str("absolute")
  final val active                = Str("active")
  final val alias                 = Str("alias")
  final val all                   = Str("all")
  final val all_petite_caps       = Str("all-petite-caps")
  final val all_scroll            = Str("all-scroll")
  final val all_small_caps        = Str("all-small-caps")
  final val always                = Str("always")
  final val auto                  = Str("auto")
  final val available             = Str("available")
  final val avoid                 = Str("avoid")
  final val avoid_column          = Str("avoid-column")
  final val avoid_page            = Str("avoid-page")
  final val balance               = Str("balance")
  final val baseline              = Str("baseline")
  final val bidi_override         = Str("bidi-override")
  final val blink                 = Str("blink")
  final val block                 = Str("block")
  final val block_end             = Str("block-end")
  final val block_start           = Str("block-start")
  final val bold                  = Str("bold")
  final val bolder                = Str("bolder")
  final val border_box            = Str("border-box")
  final val both                  = Str("both")
  final val bottom                = Str("bottom")
  final val break_all             = Str("break-all")
  final val break_word            = Str("break-word")
  final val capitalize            = Str("capitalize")
  final val caption               = Str("caption")
  final val cell                  = Str("cell")
  final val center                = Str("center")
  final val clip                  = Str("clip")
  final val clone_                = Str("clone")
  final val close_quote           = Str("close-quote")
  final val collapse              = Str("collapse")
  final val col_resize            = Str("col-resize")
  final val column                = Str("column")
  final val column_reverse        = Str("column-reverse")
  final val complex               = Str("complex")
  final val condensed             = Str("condensed")
  final val contain               = Str("contain")
  final val content               = Str("content")
  final val content_box           = Str("content-box")
  final val contents              = Str("contents")
  final val context_menu          = Str("context-menu")
  final val copy                  = Str("copy")
  final val cover                 = Str("cover")
  final val crisp_edges           = Str("crisp-edges")
  final val crosshair             = Str("crosshair")
  final val dashed                = Str("dashed")
  final val default               = Str("default")
  final val disabled              = Str("disabled")
  final val dotted                = Str("dotted")
  final val double                = Str("double")
  final val each_line             = Str("each-line")
  final val ellipsis              = Str("ellipsis")
  final val embed                 = Str("embed")
  final val end                   = Str("end")
  final val e_resize              = Str("e-resize")
  final val ew_resize             = Str("ew-resize")
  final val expanded              = Str("expanded")
  final val extra_condensed       = Str("extra-condensed")
  final val extra_expanded        = Str("extra-expanded")
  final val fill                  = Str("fill")
  final val fill_available        = Str("fill-available")
  final val fit_content           = Str("fit-content")
  final val fixed                 = Str("fixed")
  final val flat                  = Str("flat")
  final val flex                  = Str("flex")
  final val flex_end              = Str("flex-end")
  final val flex_start            = Str("flex-start")
  final val flip                  = Str("flip")
  final val from_image            = Str("from-image")
  final val full_width            = Str("full-width")
  final val grab                  = Str("grab")
  final val grabbing              = Str("grabbing")
  final val grid                  = Str("grid")
  final val hanging               = Str("hanging")
  final val help                  = Str("help")
  final val hidden                = Str("hidden")
  final val hide                  = Str("hide")
  final val historical_forms      = Str("historical-forms")
  final val horizontal            = Str("horizontal")
  final val horizontal_tb         = Str("horizontal-tb")
  final val icon                  = Str("icon")
  final val inactive              = Str("inactive")
  final val inherit               = Str("inherit")
  final val initial               = Str("initial")
  final val inline                = Str("inline")
  final val inline_block          = Str("inline-block")
  final val inline_end            = Str("inline-end")
  final val inline_flex           = Str("inline-flex")
  final val inline_grid           = Str("inline-grid")
  final val inline_start          = Str("inline-start")
  final val inline_table          = Str("inline-table")
  final val inset                 = Str("inset")
  final val inside                = Str("inside")
  final val inter_character       = Str("inter-character")
  final val invert                = Str("invert")
  final val isolate               = Str("isolate")
  final val isolate_override      = Str("isolate-override")
  final val italic                = Str("italic")
  final val justify               = Str("justify")
  final val keep_all              = Str("keep-all")
  final val left                  = Str("left")
  final val lighter               = Str("lighter")
  final val line_through          = Str("line-through")
  final val list_item             = Str("list-item")
  final val loose                 = Str("loose")
  final val lowercase             = Str("lowercase")
  final val ltr                   = Str("ltr")
  final val manipulation          = Str("manipulation")
  final val manual                = Str("manual")
  final val match_parent          = Str("match-parent")
  final val max_content           = Str("max-content")
  final val menu                  = Str("menu")
  final val message_box           = Str("message-box")
  final val middle                = Str("middle")
  final val min_content           = Str("min-content")
  final val mixed                 = Str("mixed")
  final val move                  = Str("move")
  final val ne_resize             = Str("ne-resize")
  final val nesw_resize           = Str("nesw-resize")
  final val no_close_quote        = Str("no-close-quote")
  final val no_drop               = Str("no-drop")
  final val none                  = Str("none")
  final val no_open_quote         = Str("no-open-quote")
  final val normal                = Str("normal")
  final val not_allowed           = Str("not-allowed")
  final val nowrap                = Str("nowrap")
  final val n_resize              = Str("n-resize")
  final val ns_resize             = Str("ns-resize")
  final val nw_resize             = Str("nw-resize")
  final val nwse_resize           = Str("nwse-resize")
  final val oblique               = Str("oblique")
  final val open_quote            = Str("open-quote")
  final val ordinal               = Str("ordinal")
  final val outside               = Str("outside")
  final val over                  = Str("over")
  final val overline              = Str("overline")
  final val padding_box           = Str("padding-box")
  final val page                  = Str("page")
  final val pan_x                 = Str("pan-x")
  final val pan_y                 = Str("pan-y")
  final val petite_caps           = Str("petite-caps")
  final val pixelated             = Str("pixelated")
  final val plaintext             = Str("plaintext")
  final val pointer               = Str("pointer")
  final val pre                   = Str("pre")
  final val pre_line              = Str("pre-line")
  final val preserve_3d           = Str("preserve-3d")
  final val pre_wrap              = Str("pre-wrap")
  final val progress              = Str("progress")
  final val relative              = Str("relative")
  final val repeat                = Str("repeat")
  final val right                 = Str("right")
  final val round                 = Str("round")
  final val row                   = Str("row")
  final val row_resize            = Str("row-resize")
  final val row_reverse           = Str("row-reverse")
  final val rtl                   = Str("rtl")
  final val ruby                  = Str("ruby")
  final val ruby_base             = Str("ruby-base")
  final val ruby_base_container   = Str("ruby-base-container")
  final val ruby_text             = Str("ruby-text")
  final val ruby_text_container   = Str("ruby-text-container")
  final val run_in                = Str("run-in")
  final val scale_down            = Str("scale-down")
  final val scroll                = Str("scroll")
  final val semi_condensed        = Str("semi-condensed")
  final val semi_expanded         = Str("semi-expanded")
  final val separate              = Str("separate")
  final val se_resize             = Str("se-resize")
  final val show                  = Str("show")
  final val sideways              = Str("sideways")
  final val sideways_left         = Str("sideways-left")
  final val sideways_right        = Str("sideways-right")
  final val slashed_zero          = Str("slashed-zero")
  final val slice                 = Str("slice")
  final val small_caps            = Str("small-caps")
  final val small_caption         = Str("small-caption")
  final val smooth                = Str("smooth")
  final val solid                 = Str("solid")
  final val space_around          = Str("space-around")
  final val space_between         = Str("space-between")
  final val s_resize              = Str("s-resize")
  final val start                 = Str("start")
  final val start_end             = Str("start end")
  final val static                = Str("static")
  final val status_bar            = Str("status-bar")
  final val sticky                = Str("sticky")
  final val stretch               = Str("stretch")
  final val strict                = Str("strict")
  final val style                 = Str("style")
  final val sub                   = Str("sub")
  final val super_                = Str("super")
  final val sw_resize             = Str("sw-resize")
  final val table                 = Str("table")
  final val table_cell            = Str("table-cell")
  final val table_column          = Str("table-column")
  final val table_column_group    = Str("table-column-group")
  final val table_footer_group    = Str("table-footer-group")
  final val table_header_group    = Str("table-header-group")
  final val table_row             = Str("table-row")
  final val table_row_group       = Str("table-row-group")
  final val text                  = Str("text")
  final val text_bottom           = Str("text-bottom")
  final val text_top              = Str("text-top")
  final val titling_caps          = Str("titling-caps")
  final val top                   = Str("top")
  final val ultra_condensed       = Str("ultra-condensed")
  final val ultra_expanded        = Str("ultra-expanded")
  final val under                 = Str("under")
  final val underline             = Str("underline")
  final val unicase               = Str("unicase")
  final val unset                 = Str("unset")
  final val uppercase             = Str("uppercase")
  final val upright               = Str("upright")
  final val use_glyph_orientation = Str("use-glyph-orientation")
  final val vertical              = Str("vertical")
  final val vertical_lr           = Str("vertical-lr")
  final val vertical_rl           = Str("vertical-rl")
  final val vertical_text         = Str("vertical-text")
  final val visible               = Str("visible")
  final val wait_                 = Str("wait")
  final val wavy                  = Str("wavy")
  final val weight                = Str("weight")
  final val wrap                  = Str("wrap")
  final val wrap_reverse          = Str("wrap-reverse")
  final val w_resize              = Str("w-resize")
  final val zoom_in               = Str("zoom-in")
  final val zoom_out              = Str("zoom-out")

  object TypedAttrBase {
    implicit def `Be the attr you want to be!`(t: TypedAttrBase): Attr = t.attr
  }
  abstract class TypedAttrBase {
    val attr: Attr
    protected def av[V <% Value](v: V): AV = AV(attr, v)

    /**
     * The inherit CSS-value causes the element for which it is specified to take the computed value of the property from its parent element. It is allowed on every CSS property.
     *
     * For inherited properties, this reinforces the default behavior, and is only needed to override another rule.  For non-inherited properties, this specifies a behavior that typically makes relatively little sense and you may consider using initial instead, or unset on the all property.
     */
    def inherit = av(Values.inherit)

    /** The initial CSS keyword applies the initial value of a property to an element. It is allowed on every CSS property and causes the element for which it is specified to use the initial value of the property. */
    def initial = av(Values.initial)

    /** The unset CSS keyword is the combination of the initial and inherit keywords. Like these two other CSS-wide keywords, it can be applied to any CSS property, including the CSS shorthand all. This keyword resets the property to its inherited value if it inherits from its parent or to its initial value if not. In other words, it behaves like the inherit keyword in the first case and like the initial keyword in the second case. */
    def unset = av(Values.unset)
  }
}