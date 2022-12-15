package jjm.ui

import japgolly.scalajs.react.vdom.html_<^._
import japgolly.scalajs.react.vdom.TagOf
// import japgolly.scalajs.react.ReactCats._
// import japgolly.scalajs.react.ReactMonocle._
import japgolly.scalajs.react._
import japgolly.scalajs.react.extra.StateSnapshot

import scalacss.DevDefaults._
import scalacss.ScalaCssReact._

import org.scalajs.dom.html
import org.scalajs.dom.ext.KeyCode

import cats.Order
import cats.implicits._
import monocle.Focus

object View {
  trait Styles extends StyleSheet.Inline {
    import dsl._

    val checkboxSpan = style(
      addClassNames("ml-3", "pl-3")
    )

    val checkbox = style(
      addClassNames("form-check-input")
    )

    val checkboxLabel = style(
      addClassNames("form-check-label")
    )

    val textFieldSpan = style()
    val textFieldLabel = style()
    val textFieldInput = style()

    val invalidTextBackground = style(
      backgroundColor(rgba(255, 0, 0, 0.3))
    )
    val intArrowFieldInput = style(
      width(3.em)
    )
    val shortTextField = style(
      width(50.px)
    )

    val select = style(
      addClassNames("custom-select")
    )
  }
  object Styles {
    object Default extends Styles
  }
}

class View(val styles: View.Styles) {
  @inline private[this] final def S = styles
  private val BoolLocal = new LocalState[Boolean]
  private val StringLocal = new LocalState[String]

  object Checkbox {
    def mod(
      span: TagMod = S.checkboxSpan, // checkboxSpan
      box: TagMod = S.checkbox, // checkboxBox
      label: TagMod = S.checkboxLabel)(
      toggle: StateSnapshot[Boolean],
      labelOpt: Option[String] = None,
      didUpdate: Boolean => Callback = _ => Callback.empty
    ) = <.span(span)(
      <.input(box)(
        ^.`type` := "checkbox",
        labelOpt.whenDefined(^.value := _),
        ^.checked := toggle.value,
        ^.onChange --> (toggle.modState(!_) >> didUpdate(!toggle.value))
      ),
      labelOpt.fold(TagMod.empty)(l => <.span(label)(l))
    )

    def apply(
      toggle: StateSnapshot[Boolean],
      labelOpt: Option[String],
      didUpdate: Boolean => Callback
    ): TagOf[html.Span] = mod()(toggle, labelOpt, didUpdate)

    def apply(
      toggle: StateSnapshot[Boolean],
      labelOpt: Option[String],
    ): TagOf[html.Span] = mod()(toggle, labelOpt, _ => Callback.empty)

    def apply(
      toggle: StateSnapshot[Boolean],
      didUpdate: Boolean => Callback
    ): TagOf[html.Span] = mod()(toggle, None, didUpdate)

    def apply(
      toggle: StateSnapshot[Boolean]
    ): TagOf[html.Span] = mod()(toggle, None, _ => Callback.empty)

    def apply(
      toggle: StateSnapshot[Boolean],
      label: String,
      didUpdate: Boolean => Callback
    ): TagOf[html.Span] = mod()(toggle, Some(label), didUpdate)

    def apply(
      toggle: StateSnapshot[Boolean],
      label: String
    ): TagOf[html.Span] = mod()(toggle, Some(label), _ => Callback.empty)
  }

  // TODO add styling and functionality from livetextfield
  case class TextField[A](readValue: String => Option[A], renderValue: A => String) {
    def apply(
      value: StateSnapshot[A],
      label: Option[String] = None,
    ) = <.span(
      label.whenDefined, // TODO more styling
      StringLocal.make(initialValue = renderValue(value.value)) { inputText =>
        <.input(S.textFieldInput)(
          ^.`type` := "text",
          ^.value := inputText.value,
          ^.onChange ==> ((e: ReactEventFromInput) => inputText.setState(e.target.value)),
          ^.onKeyDown ==> ((e: ReactKeyboardEventFromInput) =>
            CallbackOption.keyCodeSwitch(e) {
              case KeyCode.Enter =>
                readValue(inputText.value).fold(Callback.empty)(value.setState)
            }
          )
        )
      }
    )
    def dimapOpt[B](f: A => Option[B], g: B => A) = TextField[B](
      readValue.map(_.flatMap(f)), b => renderValue(g(b))
    )
  }
  object TextField {
    def String = TextField[String](Option(_), _.toString)
    def Double = TextField[Double]((s: String) => scala.util.Try(s.toDouble).toOption, _.toString)
  }

  case class LiveTextField[A](readValue: String => Option[A], renderValue: A => String) {

    def modInput(
      input: TagMod = S.textFieldInput,
      inputFromValue: Option[A] => TagMod = LiveTextField.defaultInputStyleFromValue[A](_))(
      value: StateSnapshot[A],
      placeholderOpt: Option[String] = None,
      didUpdateValue: A => Callback = _ => Callback.empty
    ): VdomElement = StringLocal.make(
      initialValue = renderValue(value.value),
      shouldRefresh = (s => readValue(s).forall(_ != value.value))
    ) { inputText =>
      BoolLocal.make(initialValue = false) { isInvalid =>
        <.input(input, inputFromValue(Option(value.value).filter(_ => !isInvalid.value)))(
          ^.`type` := "text",
          placeholderOpt.whenDefined(ph => ^.placeholder := ph),
          ^.value := inputText.value,
          ^.onChange ==> ((e: ReactEventFromInput) =>
            inputText.setState(e.target.value) >>
              readValue(e.target.value).fold(isInvalid.setState(true))(v =>
                isInvalid.setState(false) >> value.setState(v) >> didUpdateValue(v)
              )
          )
        )
      }
    }

    def modSpan(
      span: TagMod = S.textFieldSpan,
      label: TagMod = S.textFieldLabel,
      input: TagMod = S.textFieldInput,
      inputFromValue: Option[A] => TagMod = LiveTextField.defaultInputStyleFromValue[A](_))(
      value: StateSnapshot[A],
      labelOpt: Option[String] = None,
      placeholderOpt: Option[String] = None,
      didUpdateValue: A => Callback = _ => Callback.empty
    ) = <.span(span)(
      labelOpt.whenDefined(l => <.label(label)(s" $l")),
      modInput(input, inputFromValue)(value, placeholderOpt, didUpdateValue)
    )

    def modDiv(
      div: TagMod = S.textFieldSpan,
      label: TagMod = S.textFieldLabel,
      input: TagMod = S.textFieldInput,
      inputFromValue: Option[A] => TagMod = LiveTextField.defaultInputStyleFromValue[A](_))(
      value: StateSnapshot[A],
      labelOpt: Option[String] = None,
      placeholderOpt: Option[String] = None,
      didUpdateValue: A => Callback = _ => Callback.empty
    ) = <.div(div)(
      labelOpt.whenDefined(l => <.label(label)(s" $l")),
      modInput(input, inputFromValue)(value, placeholderOpt, didUpdateValue)
    )

    def div(
      value: StateSnapshot[A],
      labelOpt: Option[String] = None,
      placeholderOpt: Option[String] = None,
      didUpdateValue: A => Callback = _ => Callback.empty
    ) = modDiv()(value, labelOpt, placeholderOpt, didUpdateValue)

    def span(
      value: StateSnapshot[A],
      labelOpt: Option[String] = None,
      placeholderOpt: Option[String] = None,
      didUpdateValue: A => Callback = _ => Callback.empty
    ) = modSpan()(value, labelOpt, placeholderOpt, didUpdateValue)

    def input(
      value: StateSnapshot[A],
      placeholderOpt: Option[String] = None,
      didUpdateValue: A => Callback = _ => Callback.empty
    ) = modInput()(value, placeholderOpt, didUpdateValue)

    // use span for backwards compatibility
    def mod(
      span: TagMod = S.textFieldSpan,
      label: TagMod = S.textFieldLabel,
      input: TagMod = S.textFieldInput,
      inputFromValue: Option[A] => TagMod = LiveTextField.defaultInputStyleFromValue[A](_))(
      value: StateSnapshot[A],
      labelOpt: Option[String] = None,
      placeholderOpt: Option[String] = None,
      didUpdateValue: A => Callback = _ => Callback.empty
    ) = modSpan(span, label, input, inputFromValue)(value, labelOpt, placeholderOpt, didUpdateValue)

    def apply(
      value: StateSnapshot[A],
      labelOpt: Option[String] = None,
      placeholderOpt: Option[String] = None,
      didUpdateValue: A => Callback = _ => Callback.empty
    ) = span(value, labelOpt, placeholderOpt, didUpdateValue)
}
  object LiveTextField {
    def Double = LiveTextField[Double]((s: String) => scala.util.Try(s.toDouble).toOption, _.toString)
    def String = LiveTextField[String](Option(_), _.toString)

    def defaultInputStyleFromValue[A](valueOpt: Option[A]): TagMod =
      S.invalidTextBackground.when(valueOpt.isEmpty)
  }

  case class LiveTextArea[A](readValue: String => Option[A], renderValue: A => String) {

    def mod(
      span: TagMod = S.textFieldSpan,
      label: TagMod = S.textFieldLabel,
      textarea: TagMod = S.textFieldInput,
      textareaFromValue: Option[A] => TagMod = LiveTextArea.defaultInputStyleFromValue[A](_))(
      value: StateSnapshot[A],
      labelOpt: Option[String] = None,
      placeholderOpt: Option[String] = None,
      didUpdateValue: A => Callback = _ => Callback.empty
    ) = <.span(span)(
      labelOpt.whenDefined(l => <.span(label)(s" $l")),
      StringLocal.make(initialValue = renderValue(value.value)) { inputText =>
        BoolLocal.make(initialValue = false) { isInvalid =>
          <.textarea(textarea, textareaFromValue(Option(value.value).filter(_ => !isInvalid.value)))(
            placeholderOpt.whenDefined(ph => ^.placeholder := ph),
            ^.value := inputText.value,
            ^.onChange ==> ((e: ReactEventFromInput) =>
              inputText.setState(e.target.value) >>
                readValue(e.target.value).fold(isInvalid.setState(true))(v =>
                  isInvalid.setState(false) >> value.setState(v) >> didUpdateValue(v)
                )
            )
          )
        }
      }
    )

    def apply(
      value: StateSnapshot[A],
      labelOpt: Option[String] = None,
      placeholderOpt: Option[String] = None,
      didUpdateValue: A => Callback = _ => Callback.empty
    ) = mod()(value, labelOpt, placeholderOpt, didUpdateValue)
  }
  object LiveTextArea {
    def Double = LiveTextArea[Double]((s: String) => scala.util.Try(s.toDouble).toOption, _.toString)
    def String = LiveTextArea[String](Option(_), _.toString)

    def defaultInputStyleFromValue[A](valueOpt: Option[A]): TagMod =
      S.invalidTextBackground.when(valueOpt.isEmpty)
  }

  object NumberField {
    def mod(
      span: TagMod = TagMod.empty,
      label: TagMod = TagMod.empty,
      input: TagMod = S.intArrowFieldInput)(
      value: StateSnapshot[Int],
      labelOpt: Option[String] = None,
      didUpdate: Int => Callback = _ => Callback.empty
    ) = {
      <.span(span)(
        labelOpt.whenDefined(l => <.span(label)(s"$l: ")),
        <.input(input)(
          ^.`type` := "number",
          ^.min := 1,
          ^.value := value.value,
          ^.onChange ==> { (e: ReactEventFromInput) =>
            val newVal = e.target.value.toInt
            value.setState(newVal) >> didUpdate(newVal)
          }
        )
      )
    }
    def apply(
      value: StateSnapshot[Int],
      labelOpt: Option[String] = None,
      didUpdate: Int => Callback = _ => Callback.empty
    ) = mod()(value, labelOpt, didUpdate)
  }

  object Slider {
    def mod(
      span: TagMod = TagMod.empty,
      slider: TagMod = TagMod.empty,
      textSpan: TagMod = TagMod.empty,
      textInput: TagMod = S.shortTextField,
      textInputFromValue: Option[Double] => TagMod = LiveTextField.defaultInputStyleFromValue[Double](_),
      label: TagMod = TagMod.empty)(
      value: StateSnapshot[Double],
      min: Double,
      max: Double,
      labelOpt: Option[String] = None,
      numSigFigs: Int = 3,
      didUpdate: Double => Callback = _ => Callback.empty
    ) = {
      val magnitude = scala.math.pow(10, numSigFigs).toInt
      def int(x: Double) = (x * magnitude).toInt
      def double(n: Int) = n.toDouble / magnitude
      <.span(span)(
        LiveTextField.Double.mod(
          span = textSpan,
          input = textInput,
          inputFromValue = textInputFromValue,
          label = label)(
          value, labelOpt),
        " ",
        <.input(slider)(
          ^.`type` := "range",
          ^.min := int(min),
          ^.max := int(max),
          ^.value := int(value.value),
          ^.onChange ==> { (e: ReactEventFromInput) =>
            val newVal = double(e.target.value.toInt)
            value.setState(newVal) >> didUpdate(newVal)
          }
        )
      )
    }

    def apply(
      value: StateSnapshot[Double],
      min: Double,
      max: Double,
      labelOpt: Option[String] = None,
      numSigFigs: Int = 3,
      didUpdate: Double => Callback = _ => Callback.empty
    ) = mod()(value, min, max, labelOpt, numSigFigs, didUpdate)
  }

  case class OptionalSelect[A: Order](
    show: A => String,
    none: String = "None") {

    def modFull(select: TagMod = S.select)(
      choices: Set[A],
      curChoice: Option[A],
      setChoice: Option[A] => Callback
    ): TagOf[html.Select] = <.select(select)(
      ^.value := curChoice.fold(none)(show),
      ^.onChange ==> (
        (e: ReactEventFrom[org.scalajs.dom.html.Select]) => {
          val valStr = e.target.value
          val value = {
            if(valStr == none) None
            else choices.find(c => show(c) == valStr)
          }
          if(value != curChoice) setChoice(value) else Callback.empty
        }
      ),
      <.option(^.key := none, ^.value := none, none),
      choices.toList.sorted.map(show).zipWithIndex.toVdomArray { case (c, i) =>
        <.option(^.key := s"$c-$i", ^.value := c, c)
      }
    )

    def mod(select: TagMod = S.select)(
      choices: Set[A],
      choice: StateSnapshot[Option[A]]
    ): TagOf[html.Select] = modFull(select)(choices, choice.value, choice.setState)

    def apply(
      choices: Set[A],
      curChoice: Option[A],
      setChoice: Option[A] => Callback
    ): TagOf[html.Select] = {
      modFull()(choices, curChoice, setChoice)
    }

    def apply(choices: Set[A], choice: StateSnapshot[Option[A]]): TagOf[html.Select] = {
      mod()(choices, choice)
    }

    def contramap[B: Order](f: B => A) = OptionalSelect(f andThen show, none)
  }

  case class Select[A](show: A => String) {
    def modFull(select: TagMod = S.select)(
      choices: List[A],
      curChoice: A, setChoice: A => Callback,
    ): TagOf[html.Select] = <.select(select)(
      ^.value := show(curChoice),
      ^.onChange ==> (
        (e: ReactEventFrom[org.scalajs.dom.html.Select]) => {
          val valStr = e.target.value
          val value = choices.find(c => show(c) == valStr).get
          if(value != curChoice) setChoice(value) else Callback.empty
        }
      ),
      choices.map(show).zipWithIndex.toVdomArray { case (c, i) =>
        <.option(^.key := s"$c-$i", ^.value := c, c)
      }
    )

    def mod(select: TagMod = S.select)(
      choices: List[A],
      choice: StateSnapshot[A],
    ): TagOf[html.Select] = modFull(select)(choices, choice.value, choice.setState)

    def apply(
      choices: List[A],
      curChoice: A, setChoice: A => Callback
    ): TagOf[html.Select] = modFull()(choices, curChoice, setChoice)

    def apply(
      choices: List[A],
      choice: StateSnapshot[A]
    ): TagOf[html.Select] = mod()(choices, choice)

    def contramap[B](f: B => A): Select[B] = Select(f andThen show)
  }
  object Select {
    val String = Select[String](identity)
  }

  import jjm.ling.ESpan
  import jjm.ling.Span
  import jjm.ling.Text
  import jjm.ling.TokenText

  import cats.data.NonEmptyList
  import cats.implicits._

  import scalajs.js

  object Spans {

    def renderHighlightedTokens(
      tokens: Vector[String],
      highlights: List[(Span, Rgba)],
      wordRenderers : Map[Int, VdomTag => VdomTag] = Map()
    ) = {
      val wordIndexToLayeredColors = (0 until tokens.size).map { i =>
        i -> highlights.collect {
          case (span, color) if span.contains(i) => color
        }
      }.toMap
      val indexAfterToSpaceLayeredColors = (1 to tokens.size).map { i =>
        i -> highlights.collect {
          case (span, color) if span.contains(i - 1) && span.contains(i) => color
        }
      }.toMap
      Text.renderTokens[Int, List, List[VdomElement]](
        words = tokens.indices.toList,
        getToken = (index: Int) => tokens(index),
        spaceFromNextWord = (nextIndex: Int) => {
          val colors = indexAfterToSpaceLayeredColors(nextIndex)
          val colorStr = NonEmptyList[Rgba](Rgba.transparent, colors)
            .reduce((x: Rgba, y: Rgba) => x add y).toColorStyleString
          List(
            <.span(
              ^.key := s"space-$nextIndex",
              ^.style := js.Dynamic.literal("backgroundColor" -> colorStr),
              " "
            )
          )
        },
        renderWord = (index: Int) => {
          val colorStr = NonEmptyList(Rgba.transparent, wordIndexToLayeredColors(index))
            .reduce((x: Rgba, y: Rgba) => x add y).toColorStyleString
          val render: (VdomTag => VdomTag) = wordRenderers.get(index).getOrElse((x: VdomTag) => x)
          val element: VdomTag = render(
            <.span(
              ^.style := js.Dynamic.literal("backgroundColor" -> colorStr),
              Text.normalizeToken(tokens(index))
            )
          )
          List(element(^.key := s"word-$index"))
        }
      ).toVdomArray(x => x)
    }

    def renderHighlightedTokenText(
      tokens: Vector[TokenText],
      highlights: List[(Span, Rgba)],
      wordRenderers : Map[Int, VdomTag => VdomTag] = Map()
    ) = {
      val wordIndexToLayeredColors = (0 until tokens.size).map { i =>
        i -> highlights.collect {
          case (span, color) if span.contains(i) => color
        }
      }.toMap
      val indexAfterToSpaceLayeredColors = (0 to tokens.size).map { i =>
        i -> highlights.collect {
          case (span, color) if span.contains(i - 1) && span.contains(i) => color
        }
      }.toMap
      Text.renderSource[Int, List, List[VdomElement]](
        words = tokens.indices.toList,
        getSourceText = (index: Int) => tokens(index),
        spaceFromSurroundingWords = (prevIndex: Option[Int], space: String, nextIndex: Option[Int]) => {
          if(space.isEmpty) Nil else {
            val colors = nextIndex.foldMap(indexAfterToSpaceLayeredColors)
            val colorStr = NonEmptyList[Rgba](Rgba.transparent, colors)
              .reduce((x: Rgba, y: Rgba) => x add y).toColorStyleString
            List(
              <.span(
                ^.key := s"space-$nextIndex",
                ^.style := js.Dynamic.literal("backgroundColor" -> colorStr),
                space
              )
            )
          }
        },
        renderWord = (index: Int) => {
          val colorStr = NonEmptyList(Rgba.transparent, wordIndexToLayeredColors(index))
            .reduce((x: Rgba, y: Rgba) => x add y).toColorStyleString
          val render: (VdomTag => VdomTag) = wordRenderers.get(index).getOrElse((x: VdomTag) => x)
          val element: VdomTag = render(
            <.span(
              ^.style := js.Dynamic.literal("backgroundColor" -> colorStr),
              tokens(index).token
            )
          )
          List(element(^.key := s"word-$index"))
        }
      ).toVdomArray(x => x)
    }

    import monocle.Lens
    import monocle.function.{all => Optics}

    def cropPassageAndSpans[T, A](
      passage: Vector[T],
      spanItems: NonEmptyList[A],
      spanLens: Lens[A, Span]
    ): (Vector[T], NonEmptyList[A]) = {
      val min = spanItems.map(s => spanLens.get(s).begin).minimum
      val max = spanItems.map(s => spanLens.get(s).endExclusive).maximum
      passage.slice(min, max) -> spanItems.map(spanLens.modify(_.translate(-min)))
    }

    def renderTokenHighlights(
      tokens: Vector[String],
      highlights: NonEmptyList[(ESpan, Rgba)]
    ): VdomArray = {
      val orderedHighlights = highlights.sortBy(_._1)
      case class GroupingState(
        completeGroups: List[NonEmptyList[(ESpan, Rgba)]],
        currentGroup: NonEmptyList[(ESpan, Rgba)]
      )
      val groupingState = orderedHighlights.tail
        .foldLeft(GroupingState(Nil, NonEmptyList.of(orderedHighlights.head))) {
        case (GroupingState(groups, curGroup), (span, color)) =>
          if(curGroup.exists(_._1.overlaps(span))) {
            GroupingState(groups, (span -> color) :: curGroup)
          } else {
            GroupingState(curGroup :: groups, NonEmptyList.of(span -> color))
          }
      }
      val contigSpanLists = NonEmptyList(groupingState.currentGroup, groupingState.completeGroups)
      val answerHighlighties = contigSpanLists.reverse.map { spanList =>
        val (groupTokens, groupSpans) = cropPassageAndSpans(
          tokens, spanList, Focus[(Span, Rgba)](_._1)
        )
        List(
          <.span(
            renderHighlightedTokens(groupTokens, groupSpans.toList)
          )
        )
      }.intercalate(List(<.span(" / ")))
      answerHighlighties.zipWithIndex.toVdomArray { case (a, i) =>
        a(^.key := s"answerString-$i")
      }
    }

    def renderTokenTextHighlights(
      tokens: Vector[TokenText],
      highlights: NonEmptyList[(ESpan, Rgba)]
    ): VdomArray = {
      val orderedHighlights = highlights.sortBy(_._1)
      case class GroupingState(
        completeGroups: List[NonEmptyList[(ESpan, Rgba)]],
        currentGroup: NonEmptyList[(ESpan, Rgba)]
      )
      val groupingState = orderedHighlights.tail
        .foldLeft(GroupingState(Nil, NonEmptyList.of(orderedHighlights.head))) {
        case (GroupingState(groups, curGroup), (span, color)) =>
          if(curGroup.exists(_._1.overlaps(span))) {
            GroupingState(groups, (span -> color) :: curGroup)
          } else {
            GroupingState(curGroup :: groups, NonEmptyList.of(span -> color))
          }
      }
      val contigSpanLists = NonEmptyList(groupingState.currentGroup, groupingState.completeGroups)
      val answerHighlighties = contigSpanLists.reverse.map { spanList =>
        val (groupTokens, groupSpans) = cropPassageAndSpans(
          tokens, spanList, Focus[(Span, Rgba)](_._1)
        )
        List(
          <.span(
            renderHighlightedTokenText(groupTokens, groupSpans.toList)
          )
        )
      }.intercalate(List(<.span(" / ")))
      answerHighlighties.zipWithIndex.toVdomArray { case (a, i) =>
        a(^.key := s"answerString-$i")
      }
    }
  }
}
