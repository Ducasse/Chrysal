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

{ #category : #'as yet unclassified' }
BooleanConfigurationItem >> beFalseAsDefault [
	"returns a domain object"
	defaultDomainValue := false
]

{ #category : #'as yet unclassified' }
BooleanConfigurationItem >> beTrueAsDefault [

	defaultDomainValue := true
]

{ #category : #accessing }
BooleanConfigurationItem >> defaultDomainValue [
	^ defaultDomainValue 
]

{ #category : #accessing }
BooleanConfigurationItem >> defaultDomainValueString [
	^ defaultDomainValue asString
]

{ #category : #'as yet unclassified' }
BooleanConfigurationItem >> possibleValuesString [
	^ #('true' 'false')
	
]
