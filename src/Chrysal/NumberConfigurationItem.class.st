"
To describe number items.
Literal in STON.

(NumberConfigurationItem new
		propertyName: 'headingLevelOffset';
		default: 0;
		yourself)
"
Class {
	#name : 'NumberConfigurationItem',
	#superclass : 'ChrysalItem',
	#category : 'Chrysal-Model',
	#package : 'Chrysal',
	#tag : 'Model'
}

{ #category : 'accessing' }
NumberConfigurationItem >> defaultDomainValueString [
	"Returns a string representing the default value but as an object once imported in Pharo and not a string used by the writer of a configuration."

	^ self default asString
]

{ #category : 'accessing' }
NumberConfigurationItem >> domainValueConversionString [
	"Returns a string representing the conversion of a value from the configuration writer world to Pharo. aValue is the parameter representing the object to be converted"

	^ '^ aValue'
]
