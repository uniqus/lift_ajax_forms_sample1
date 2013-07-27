package code.snippet

import scala.xml.{ NodeSeq, Text }
import net.liftweb.util._
import net.liftweb.common._
import java.util.Date
import code.lib._
import Helpers._
import net.liftweb.http.{ SHtml, S }
import net.liftweb.http.js.JsCmds
import scala.collection.mutable.ArrayBuffer
import net.liftweb.http.Templates
import net.liftweb.json.JsonDSL._
import net.liftweb.json.JsonAST._
import net.liftweb.http.JsonResponse
import net.liftweb.http.LiftRules
import net.liftweb.http.js.JE

final case class SampleItem(id: Long, name: String)

class MySampleForm extends Loggable {
  lazy val sampleItems = List(SampleItem(1, "Item #1"), SampleItem(2, "Item #2"), SampleItem(3, "Item #3"), SampleItem(4, "And one more to be sure"), SampleItem(5, "Well, the last one"), SampleItem(6, "Stop me please!"))
  lazy val selectedItemIds = ArrayBuffer[Long]()
  //  protected var dataTableColumnDef: Seq[String] = Seq()

  def renderItem(item: SampleItem): NodeSeq => NodeSeq = {
    ":checkbox" #> SHtml.checkbox(selectedItemIds.contains(item.id), checked => {
      if (checked) {
        logger.info("Selected item ID: " + item.id)
        selectedItemIds += item.id
      }
    }) &
      ".item-name" #> item.name &
      ".item-id" #> item.id
  }

  def renderDataTable(in: NodeSeq): NodeSeq = {
    // This template is used to render single item
    val itemTemplate: NodeSeq = <td><input type="checkbox"/></td><td><span class="item-id"></span></td><td><span class="item-name"></span></td>;

    val getJSONRows = (ignore: String) => {
      // This function provides data to DataTables via AJAX request
      val json = ("iTotalRecords" -> sampleItems.length) ~
        ("iTotalDisplayRecords" -> sampleItems.length) ~
        ("sEcho" -> S.param("sEcho").dmap(0)(_.toInt)) ~
        ("aaData" -> sampleItems.map(
          /* Here we take a list of sampleItes, and rendering them to
             * List[NodeSeq] using renderItem transform on itemTemplate for each item */
          item => renderItem(item).apply(itemTemplate).map(
            /* Here we just transform each rendered item NodeSeq to string to pass it as a JSON array of strings */
              /* TD is stripped by using t.child instead of t */
            renderedHtml => JString(renderedHtml.child.map(a => Html5.toString(a)).reduce((a, b) => a + b)))))
      JsonResponse(json)
    /*
     * The resulting JSON will look like:
{
  "iTotalRecords":6,
  "iTotalDisplayRecords":6,
  "sEcho":2,
  "aaData":[
	  ["<input type=\"checkbox\" name=\"F750933123242NG3E2O\" value=\"true\"><input type=\"hidden\" name=\"F750933123242NG3E2O\" value=\"false\">","1","Item #1"],
	  ["<input type=\"checkbox\" name=\"F750933123243RCFCHR\" value=\"true\"><input type=\"hidden\" name=\"F750933123243RCFCHR\" value=\"false\">","2","Item #2"],
	  ...
  ]
}
     *   
     * 
     */
      
    }

    S.fmapFunc(S.SFuncHolder(getJSONRows)) { func =>
      val where: String = S.encodeURL(S.contextPath + "/" + LiftRules.ajaxPath + "?" + func + "=foo")

      // Here we are passing mapped getJSONRows function name to DataTables to be able to request data via AJAX
      val dataTableConfig = """{ bProcessing: true, bServerSide: true, sAjaxSource: """ + where.encJs + "}"

      // Here we are initializing jquery DataTable using defaultConfig
      val onLoad = JE.JsRaw("""
        jQuery(document).ready(function() {
			$("#data-tables-test").dataTable(""" + dataTableConfig + """);
		});""").cmd

      in ++ <head_merge>
              { JsCmds.Script(onLoad) }
            </head_merge>

    }
  }

  def render(in: NodeSeq): NodeSeq = {
    (
      "#test-button" #> SHtml.ajaxSubmit("Test it!", () => {
        logger.info("Form submitted with selectedItemIds: " + selectedItemIds)
        JsCmds.SetHtml("test-results", <p>Selected items: { selectedItemIds }</p>)
      })).apply(renderDataTable(in))
  }

}
