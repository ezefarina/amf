Model: file://amf-client/shared/src/test/resources/validations/xmlexample/offices_xml_type.raml
Profile: RAML 1.0
Conforms? true
Number of results: 2

Level: Warning

- Source: http://a.ml/vocabularies/amf/parser#schema-deprecated
  Message: 'schema' keyword it's deprecated for 1.0 version, should use 'type' instead
  Level: Warning
  Target: 
  Property: 
  Position: Some(LexicalInformation([(8,8)-(8,14)]))
  Location: file://amf-client/shared/src/test/resources/validations/xmlexample/offices_xml_type.raml

- Source: http://a.ml/vocabularies/amf/parser#unsupported-example-media-type-warning
  Message: Unsupported validation for mediatype: application/xml and shape file://amf-client/shared/src/test/resources/validations/xmlexample/offices_xml_type.raml#/declarations/types/schema/Office
  Level: Warning
  Target: file://amf-client/shared/src/test/resources/validations/xmlexample/offices_xml_type.raml#/declarations/types/schema/Office/example/default-example
  Property: http://a.ml/vocabularies/document#value
  Position: Some(LexicalInformation([(9,17)-(9,46)]))
  Location: file://amf-client/shared/src/test/resources/validations/xmlexample/offices_xml_type.raml
