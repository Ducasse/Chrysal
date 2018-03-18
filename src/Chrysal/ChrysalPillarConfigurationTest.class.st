Class {
	#name : #ChrysalPillarConfigurationTest,
	#superclass : #TestCase,
	#category : #'Chrysal-ToBeMovedElsewhere'
}

{ #category : #tests }
ChrysalPillarConfigurationTest >> configurationClass [
	^ ConfigurationForPillar
]

{ #category : #tests }
ChrysalPillarConfigurationTest >> testReadingDefaultScriptLanguage [
	| dictionary config |
	dictionary := Dictionary with: 'defaultScriptLanguage' -> 'smalltalk'.
	config := self configurationClass newFromDictionary: dictionary.
	self assert: config defaultScriptLanguage equals: (PRAbstractScriptLanguage for: 'smalltalk')
]

{ #category : #tests }
ChrysalPillarConfigurationTest >> testReadingNewLine [
	| dictionary config |
	dictionary := Dictionary with: 'newLine' -> #unix.
	config := self configurationClass newFromDictionary: dictionary.
	self assert: config newLine equals: UnixPlatform new lineEnding
]

{ #category : #tests }
ChrysalPillarConfigurationTest >> testReadingRenderStructureAsSlide [
	| dictionary config |
	dictionary := Dictionary with: 'renderStructureAsSlide' -> false.
	config := self configurationClass newFromDictionary: dictionary.
	self assert: config renderStructureAsSlide equals: false
]

{ #category : #tests }
ChrysalPillarConfigurationTest >> testReadingScrambledEmailAddresses [
	| dictionary config |
	dictionary := STON fromStream: '{ "scrambledEmailAddresses" :"true" }' readStream.
	config := self configurationClass newFromDictionary: dictionary.
	self assert: config scrambledEmailAddresses equals: true
]

{ #category : #tests }
ChrysalPillarConfigurationTest >> testReadingTitle [
	| dictionary config |
	dictionary := Dictionary with: 'title' -> 'foo'.
	config := self configurationClass newFromDictionary: dictionary.
	self assert: config title equals: 'foo'
]

{ #category : #tests }
ChrysalPillarConfigurationTest >> testSeparateOutputFiles [
	| dictionary config |
	dictionary := STON
		fromStream: '{  "separateOutputFiles" :"false" }' readStream.
	config := self configurationClass
		newFromDictionary: dictionary.
	self assert: config separateOutputFiles equals: false
	

]
