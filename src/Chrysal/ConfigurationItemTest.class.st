Class {
	#name : #ConfigurationItemTest,
	#superclass : #TestCase,
	#category : #'Chrysal-Tests'
}

{ #category : #tests }
ConfigurationItemTest >> testBooleanConversion [
	| item |
	item := BooleanConfigurationItem new
		propertyName: 'verbose';
		default: 'false';
		yourself.
	self assert: (item convertToDomainObject: 'true') equals: true.
	self assert: (item convertToDomainObject: 'false') equals: false.
	self assert: (item convertToDomainObject: 'fals') equals: nil.
]

{ #category : #tests }
ConfigurationItemTest >> testDefaultBoolean [
	| item |
	item := StringConfigurationItem new
		propertyName: 'separateOutputFiles';
		yourself.
	self assert: item default equals: item defaultValue
]

{ #category : #tests }
ConfigurationItemTest >> testDefaultFile [
	| item |
	item := FileConfigurationItem new
		propertyName: 'mainDocument';
		yourself.
	self assert: item default equals: item defaultValue
]

{ #category : #tests }
ConfigurationItemTest >> testDefaultString [
	| item |
	item := StringConfigurationItem new propertyName: 'title' ;yourself.
	self assert: item default equals: item defaultValue
	
]

{ #category : #tests }
ConfigurationItemTest >> testDefaultString2 [
	| item |
	item :=	(StringConfigurationItem new
		propertyName: 'latexWriter';
		default: 'latex:sbabook';
		yourself).
	self assert: item defaultDomainValueString equals: '''latex:sbabook'''
	
]

{ #category : #tests }
ConfigurationItemTest >> testDefaultSymbol [
	| item |
	item := SymbolConfigurationItem new
		propertyName: 'latexWriter';
		default: #latex:sbabook;
		yourself.
	self assert: item defaultDomainValueString equals: '#latex:sbabook'
]

{ #category : #tests }
ConfigurationItemTest >> testFileConversion [
	| item |
	item := FileConfigurationItem new
		propertyName: 'mainDocument';
		yourself.
	self assert: (item convertToDomainObject: 'mainDocument') equals: FileSystem workingDirectory / 'mainDocument'
]

{ #category : #tests }
ConfigurationItemTest >> testNewLineConversion [
	| item |
	item := NewLineConfigurationItem new
		propertyName: 'newLine';
		yourself.
	self assert: item default equals: #unix
]

{ #category : #tests }
ConfigurationItemTest >> testNewLineDefault [
	| item |
	item := NewLineConfigurationItem new
		propertyName: 'newLine';
		yourself.
	self assert: (item convertToDomainObject: #unix) equals: UnixPlatform new lineEnding.
	self assert: (item convertToDomainObject: #lf) equals: UnixPlatform new lineEnding.
	self assert: (item convertToDomainObject: #crlf) equals: Win32Platform new lineEnding.
]

{ #category : #tests }
ConfigurationItemTest >> testSettingDefaultBoolean [
	| item |
	item := BooleanConfigurationItem new
		propertyName: 'separateOutputFiles';
		default: 'false';
		yourself.
	self assert: item default equals: 'false'
]

{ #category : #tests }
ConfigurationItemTest >> testSettingDefaultFile [
	| item |
	item := FileConfigurationItem new
		propertyName: 'mainDocument';
		default: 'myfile.pillar';
		yourself.
	self assert: item default equals: 'myfile.pillar'
]

{ #category : #tests }
ConfigurationItemTest >> testSettingDefaultString [
	| item |
	item := StringConfigurationItem new
		propertyName: 'title';
		default: 'my super cool book';
		yourself.
	self assert: item default equals: 'my super cool book' 
]

{ #category : #tests }
ConfigurationItemTest >> testStringConversion [
	| item |
	item := StringConfigurationItem new
		propertyName: 'latexWriter';
		yourself.
	self assert: (item convertToDomainObject: #latex:sbabook) equals: #latex:sbabook
]
