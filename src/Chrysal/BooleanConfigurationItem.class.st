Class {
	#name : #BooleanConfigurationItem,
	#superclass : #ConfigurationItem,
	#instVars : [
		'defaultDomainValue'
	],
	#category : #Chrysal
}

{ #category : #example }
BooleanConfigurationItem class >> example [
	<sampleInstance>
	^ BooleanConfigurationItem new
		propertyName: 'separateOutputFiles';
		beTrueAsDefault
]

{ #category : #'public API' }
BooleanConfigurationItem >> beFalseAsDefault [
	default := 'false'
]

{ #category : #'public API' }
BooleanConfigurationItem >> beTrueAsDefault [
	default := 'true'
]

{ #category : #accessing }
BooleanConfigurationItem >> defaultDomainValueString [
	"Returns a string representing the default value but as an object once imported in Pharo and not a string used by the writer of a configuration."

	^ self defaultValue
]

{ #category : #unused }
BooleanConfigurationItem >> possibleValuesString [
	^ #('true' 'false')
	
]
