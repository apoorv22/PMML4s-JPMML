import java.io.InputStream

import javax.xml.transform.Source
import org.dmg.pmml.FieldName
import org.jpmml.evaluator.{Computable, FieldValue, ModelEvaluatorFactory}
import org.jpmml.model.filters.ImportFilter
import org.jpmml.model.visitors.{LocatorNullifier, StringInterner}
import org.jpmml.model.{JAXBUtil, SAXUtil}
import org.pmml4s.model.Model
import scala.collection.JavaConverters._

final class Evaluator {

  def init(): Unit = {
    val fileInputStream = getClass.getResourceAsStream("/pmml")
    val fileInputStream2 = getClass.getResourceAsStream("/pmml")
    val map = Map(
      "Checking_Account" -> "A11",
      "Duration" -> 6,
      "Installment_per_income" -> 4,
      "Telephone" -> "A192"
    )

    println(evaluate_jpmml(fileInputStream, map))
    println(evaluate_pmml4s(fileInputStream2, map))
  }

  def evaluate_pmml4s(inputStream: InputStream, input: Map[String, Any]) = {
    val model = Model.fromInputStream(inputStream)
    model.predict(input)

  }

  def evaluate_jpmml(inputStream: InputStream, input: Map[String, Any]) = {
    val source: Source =
      SAXUtil.createFilteredSource(inputStream, new ImportFilter())
    val pmml = JAXBUtil.unmarshalPMML(source)
    new StringInterner().applyTo(pmml)

    new LocatorNullifier().applyTo(pmml)
    val evaluator = ModelEvaluatorFactory.newInstance.newModelEvaluator(pmml)
    val pmmlInputFields = evaluator.getInputFields.asScala.toSet
    val fieldNameToFieldValueMap: Map[FieldName, FieldValue] =
      pmmlInputFields.foldLeft(Map.empty[FieldName, FieldValue]) {
        case (acc, field) =>
          val fieldName = field.getName
          val columnValue = input(fieldName.getValue)
          acc + (fieldName -> field.prepare(columnValue))
      }
    val columnValuesMap =
      evaluator
        .evaluate(fieldNameToFieldValueMap.asJava)
        .asScala
        .foldLeft(Map.empty[String, Any]) {
          case (acc, (fieldName, fieldValue)) =>
            val primitiveValue = fieldValue match {
              case x: Computable => x.getResult
              case _             => fieldValue
            }
            acc + (fieldName.getValue -> primitiveValue)
        }
    columnValuesMap

  }
}
