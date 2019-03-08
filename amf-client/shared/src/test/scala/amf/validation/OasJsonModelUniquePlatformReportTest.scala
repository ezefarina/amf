package amf.validation

import amf.core.remote.{Hint, OasJsonHint}

class OasJsonModelUniquePlatformReportTest extends UniquePlatformReportGenTest {

  override val basePath    = "file://amf-client/shared/src/test/resources/validations/"
  override val reportsPath = "amf-client/shared/src/test/resources/validations/reports/model/"

  test("Tags in oas") {
    validate("/webapi/tags.json", Some("webapi-tags.report"))
  }

  test("Parse and validate invalid responses") {
    validate("invalid-status-code-string/api.json", Some("invalid-status-code-string-oas.report"))
  }

  test("Parameter without shape") {
    validate("parameter-without-shape/parameter-without-shape.json", Some("parameter-without-shape.report"))
  }

  test("Invalid required in oas schema") {
    validate("invalid-oas-required/invalid-oas-required.json", Some("invalid-oas-required.report"))
  }

  test("Warning when using raml security schemes") {
    validate("raml-security-in-oas.json", Some("raml-security-in-oas.report"))
  }

  test("Path parameter must have the property required defined") {
    validate("path-parameter-required/required-is-not-present.json", Some("required-is-not-present.report"))
  }

  test("Path parameters must have required set to true") {
    validate("path-parameter-required/required-set-to-false.json", Some("required-set-to-false.report"))
  }

  test("Operation ids are unique") {
    validate("duplicate-operation-ids.json", Some("duplicate-operation-ids.report"))
  }

  test("Parameters of type file defined in path with invalid consumes property") {
    validate("file-parameter-consumes/parameter-in-path.json", Some("path-file-parameter-invalid-consumes.report"))
  }

  test("Parameters of type file and binding path defined in operation with invalid consumes property") {
    validate("file-parameter-consumes/binding-path-in-operation.json",
             Some("operation-file-parameter-invalid-binding-invalid-consumes.report"))
  }

  test("Parameters of type file defined in operation with invalid consumes property") {
    validate("file-parameter-consumes/parameter-in-operation.json",
             Some("operation-file-parameter-invalid-consumes.report"))
  }

  test("Parameter of type file with no consumes property defined") {
    validate("file-parameter-consumes/no-consumes-defined.json", Some("file-parameter-no-consumes.report"))
  }

  override val hint: Hint = OasJsonHint
}
