Model: file://amf-client/shared/src/test/resources/validations/recursives/response-without-mediatype.raml
Profile: RAML 1.0
Conforms? false
Number of results: 1

Level: Violation

- Source: http://a.ml/vocabularies/amf/parser#example-validation-error
  Message: prop should be array
  Level: Violation
  Target: file://amf-client/shared/src/test/resources/validations/recursives/response-without-mediatype.raml#/web-api/end-points/%2Fendpoint/get/200/default/example/default-example
  Property: 
  Position: Some(LexicalInformation([(24,18)-(27,19)]))
  Location: file://amf-client/shared/src/test/resources/validations/recursives/response-without-mediatype.raml
