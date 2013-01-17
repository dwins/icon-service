package dwins.icons

import scala.collection.JavaConverters._

import unfiltered.request._
import unfiltered.response._

import org.clapper.avsl.Logger

/** unfiltered plan */
class IconService extends unfiltered.filter.Plan {
  import QParams._
  val logger = Logger(classOf[IconService])

  def intent = {
    case GET(Path("/")) =>
      Ok ~> PlainTextContent ~> ResponseString("hi")
    case GET(Path(Seg("st" :: styleName :: Nil))) & Params(params) =>
      def asInt(s:String) = 
        try
          Some(s.toInt)
        catch {
          case (e: NumberFormatException) => None
        }
      val expected =
        for {
          style <- readStyle(styleName).right
        } yield pointStyle(style)
      val simpleParams = params collect { case (k, Seq(v, _*)) => (k, v) }
      expected match {
        case Left(err) =>
          BadRequest ~> PlainTextContent ~> ResponseString(err)
        case Right(rule) =>
          Ok ~> ContentType("image/png") ~> ResponseBytes(png(draw(rule, simpleParams)))
      }
  }

  def pointStyle(style: Style): Style = {
    def copy
      (rule: org.geotools.styling.Rule)
      (symbolizers: Seq[org.geotools.styling.Symbolizer]): org.geotools.styling.Rule =
    {
      val newRule = styleFactory.createRule()
      newRule.setElseFilter(rule.isElseFilter)
      newRule.setFilter(rule.getFilter)
      newRule.setMaxScaleDenominator(rule.getMaxScaleDenominator)
      newRule.setMinScaleDenominator(rule.getMinScaleDenominator)
      newRule.symbolizers.addAll(symbolizers.asJava)
      newRule
    }

    def pointRule(rule: org.geotools.styling.Rule): org.geotools.styling.Rule = 
      copy(rule)(rule.symbolizers.asScala.filter { _.isInstanceOf[org.geotools.styling.PointSymbolizer] })

    val rules = 
      for {
        ft <- style.featureTypeStyles.asScala
        rule <- ft.rules.asScala
      } yield rule

    mkStyleFromRules(rules map pointRule)
  }

  def mkStyleFromRules(rules: Seq[org.geotools.styling.Rule]): Style = {
    val style = styleFactory.createStyle
    val ftStyle = styleFactory.createFeatureTypeStyle
    ftStyle.rules.addAll(rules.asJava)
    style.featureTypeStyles.add(ftStyle)
    style
  }

  def mkStyle(syms: Seq[org.geotools.styling.Symbolizer]): Style = {
    val style = styleFactory.createStyle
    val ftStyle = styleFactory.createFeatureTypeStyle
    ftStyle.rules.add(mkRule(syms))
    style.featureTypeStyles.add(ftStyle)
    style
  }

  def mkRule(syms: Seq[org.geotools.styling.Symbolizer]): org.geotools.styling.Rule = {
    val rule = styleFactory.createRule()
    syms.foreach(rule.symbolizers.add(_))
    rule
  }

  def summarize(s: Style): String = {
    val ruleNames =
      for {
        (ftStyle, ftIndex) <- s.featureTypeStyles.asScala.zipWithIndex
        (rule, ruleIndex) <- ftStyle.rules.asScala.zipWithIndex
        symbolizerText = 
          if (rule.symbolizers.asScala.exists { _.isInstanceOf[org.opengis.style.PointSymbolizer] })
            "Pointy"
          else
            "Not pointy"
      } yield ("%d.%d %s: %s" format(ftIndex, ruleIndex, rule.getName, symbolizerText))
    ruleNames.mkString("\n")
  }

  def png(image: java.awt.image.BufferedImage): Array[Byte] = {
    val out = new java.io.ByteArrayOutputStream
    javax.imageio.ImageIO.write(image, "PNG", out)
    out.toByteArray
  }

  def graphics(s: org.geotools.styling.Symbolizer): Seq[org.opengis.style.GraphicalSymbol] = {
    def graphicSymbols(g: org.opengis.style.Graphic) = 
      g match {
        case null => Nil
        case g => g.graphicalSymbols.asScala
      }

    def strokeSymbols(s: org.geotools.styling.Stroke) =
      s match {
        case null => Nil
        case s => graphicSymbols(s.getGraphicFill) ++ graphicSymbols(s.getGraphicStroke)
      }

    def fillSymbols(f: org.geotools.styling.Fill) =
      f match {
        case null => Nil
        case f => graphicSymbols(f.getGraphicFill)
      }

    s match {
      case (p: org.geotools.styling.PointSymbolizer) => 
        graphicSymbols(p.getGraphic)
      // For KML, only points can have custom graphics anyway.
      // case (l: org.geotools.styling.LineSymbolizer) =>
      //   strokeSymbols(l.getStroke)
      // case (pl: org.geotools.styling.PolygonSymbolizer) =>
      //   fillSymbols(pl.getFill) ++ strokeSymbols(pl.getStroke)
      case _ =>
        Nil
    }
  }

}
