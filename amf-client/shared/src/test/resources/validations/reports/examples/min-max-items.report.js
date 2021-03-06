Model: file://amf-client/shared/src/test/resources/validations/examples/min-max-items.raml
Profile: RAML 1.0
Conforms? false
Number of results: 2

Level: Violation

- Source: http://a.ml/vocabularies/amf/parser#example-validation-error
  Message: should NOT have less than 2 items
  Level: Violation
  Target: file://amf-client/shared/src/test/resources/validations/examples/min-max-items.raml#/declarations/types/array/Colors/example/bad-min
  Property: 
  Position: Some(LexicalInformation([(10,15)-(10,21)]))
  Location: file://amf-client/shared/src/test/resources/validations/examples/min-max-items.raml

- Source: http://a.ml/vocabularies/amf/parser#example-validation-error
  Message: should NOT have more than 3 items
  Level: Violation
  Target: file://amf-client/shared/src/test/resources/validations/examples/min-max-items.raml#/declarations/types/array/Colors/example/bad-max
  Property: 
  Position: Some(LexicalInformation([(11,15)-(11,39)]))
  Location: file://amf-client/shared/src/test/resources/validations/examples/min-max-items.raml
