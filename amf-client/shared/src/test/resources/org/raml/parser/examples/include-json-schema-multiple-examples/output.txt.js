Model: file://amf-client/shared/src/test/resources/org/raml/parser/examples/include-json-schema-multiple-examples/input.raml
Profile: RAML 1.0
Conforms? false
Number of results: 1

Level: Violation

- Source: http://a.ml/vocabularies/amf/parser#example-validation-error
  Message: age should be integer
name should be string

  Level: Violation
  Target: file://amf-client/shared/src/test/resources/org/raml/parser/examples/include-json-schema-multiple-examples/input.raml#/declarations/types/User/example/bob
  Property: file://amf-client/shared/src/test/resources/org/raml/parser/examples/include-json-schema-multiple-examples/input.raml#/declarations/types/User/example/bob
  Position: Some(LexicalInformation([(8,10)-(11,11)]))
  Location: file://amf-client/shared/src/test/resources/org/raml/parser/examples/include-json-schema-multiple-examples/input.raml
