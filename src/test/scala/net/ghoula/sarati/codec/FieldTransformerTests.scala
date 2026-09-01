package net.ghoula.sarati.codec

import munit.FunSuite

class FieldTransformerTests extends FunSuite {

  test("IdentityFieldTransformer returns field names unchanged") {
    val transformer = IdentityFieldTransformer

    assertEquals(transformer.transformFieldName("userName"), "userName")
    assertEquals(transformer.transformFieldName("age"), "age")
    assertEquals(transformer.shouldIncludeField("anyField"), true)
  }

  test("SnakeCase transformer converts camelCase to snake_case") {
    val transformer = FieldTransformers.SnakeCase

    assertEquals(transformer.transformFieldName("userName"), "user_name")
    assertEquals(transformer.transformFieldName("userAge"), "user_age")
    assertEquals(transformer.transformFieldName("isAdmin"), "is_admin")
    assertEquals(transformer.transformFieldName("name"), "name")
    assertEquals(transformer.transformFieldName("firstName"), "first_name")
    assertEquals(transformer.shouldIncludeField("anyField"), true)
  }

  test("KebabCase transformer converts camelCase to kebab-case") {
    val transformer = FieldTransformers.KebabCase

    assertEquals(transformer.transformFieldName("userName"), "user-name")
    assertEquals(transformer.transformFieldName("userAge"), "user-age")
    assertEquals(transformer.transformFieldName("isAdmin"), "is-admin")
    assertEquals(transformer.transformFieldName("name"), "name")
    assertEquals(transformer.transformFieldName("firstName"), "first-name")
  }

  test("ScreamingSnakeCase transformer converts to SCREAMING_SNAKE_CASE") {
    val transformer = FieldTransformers.ScreamingSnakeCase

    assertEquals(transformer.transformFieldName("userName"), "USER_NAME")
    assertEquals(transformer.transformFieldName("userAge"), "USER_AGE")
    assertEquals(transformer.transformFieldName("isAdmin"), "IS_ADMIN")
    assertEquals(transformer.transformFieldName("name"), "NAME")
  }

  test("Custom transformer can add prefix to all fields") {
    val transformer = new FieldTransformer {
      def transformFieldName(fieldName: String): String = s"api_$fieldName"
      def shouldIncludeField(fieldName: String): Boolean = true
    }

    assertEquals(transformer.transformFieldName("name"), "api_name")
    assertEquals(transformer.transformFieldName("age"), "api_age")
  }

  test("Custom transformer can filter fields by pattern") {
    val transformer = new FieldTransformer {
      def transformFieldName(fieldName: String): String = fieldName
      def shouldIncludeField(fieldName: String): Boolean =
        !fieldName.startsWith("internal")
    }

    assertEquals(transformer.shouldIncludeField("name"), true)
    assertEquals(transformer.shouldIncludeField("age"), true)
    assertEquals(transformer.shouldIncludeField("internalId"), false)
    assertEquals(transformer.shouldIncludeField("internalState"), false)
  }

  test("Custom transformer can combine multiple strategies") {
    val transformer = new FieldTransformer {
      def transformFieldName(fieldName: String): String = {
        val prefixed = s"api_$fieldName"
        prefixed.replaceAll("([A-Z])", "_$1").toLowerCase.stripPrefix("_")
      }
      def shouldIncludeField(fieldName: String): Boolean = true
    }

    assertEquals(transformer.transformFieldName("userName"), "api_user_name")
    assertEquals(transformer.transformFieldName("age"), "api_age")
  }
}
