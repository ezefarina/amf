Model: file://amf-client/shared/src/test/resources/validations/recursives/union1.raml
Profile: RAML 1.0
Conforms? false
Number of results: 1

Level: Violation

- Source: http://a.ml/vocabularies/amf/parser#example-validation-error
  Message: b should be string
b.b should be string
c should be object
c should be string
should match some schema in anyOf

  Level: Violation
  Target: file://amf-client/shared/src/test/resources/validations/recursives/union1.raml#/declarations/types/any/A/example/invalid
  Property: file://amf-client/shared/src/test/resources/validations/recursives/union1.raml#/declarations/types/any/A/example/invalid
  Position: Some(LexicalInformation([(12,0)-(15,0)]))
  Location: file://amf-client/shared/src/test/resources/validations/recursives/union1.raml
