Class {
	#name : #ConfigurationItemTest,
	#superclass : #TestCase,
	#category : #'Chrysal-Tests-Tests'
}

{ #category : #tests }
ConfigurationItemTest >> testDefaultBoolean [
	| item |
	item := BooleanConfigurationItem new
		propertyName: #separateOutputFiles;
		yourself.
	self assert: item default equals: item defaultValue
]

{ #category : #tests }
ConfigurationItemTest >> testDefaultBooleanIsAReallyBoolean [
	| item |
	item := BooleanConfigurationItem new
		propertyName: #separateOutputFiles;
		beFalseAsDefault;
		yourself.
	self assert: item default equals: 'false'.
	"this is super strange to say that it should be a boolean and to test in fact a string 
	but this is because the default value is a string that will be used to generate the ifAbsent: [ false ]"
	self assert: item defaultDomainValueString equals: 'false'.
	
]

{ #category : #tests }
ConfigurationItemTest >> testDefaultFile [
	| item |
	item := FileConfigurationItem new
		propertyName: #mainDocument;
		yourself.
	self assert: item default equals: item defaultValue
]

{ #category : #tests }
ConfigurationItemTest >> testDefaultString [
	| item |
	item := StringConfigurationItem new propertyName: #title ; yourself.
	self assert: item default equals: item defaultValue
	
]

{ #category : #tests }
ConfigurationItemTest >> testDefaultString2 [
	| item |
	item :=	(StringConfigurationItem new
		propertyName: #latexWriter;
		default: 'latex:sbabook';
		yourself).
	self assert: item defaultDomainValueString equals: '''latex:sbabook'''
	
]

{ #category : #tests }
ConfigurationItemTest >> testDefaultSymbol [
	| item |
	item := SymbolConfigurationItem new
		propertyName: #latexWriter;
		default: #latex:sbabook;
		yourself.
	self assert: item defaultDomainValueString equals: '#latex:sbabook'
]

{ #category : #tests }
ConfigurationItemTest >> testNewLineConversion [
	| item |
	item := NewLineConfigurationItem new
		propertyName: #newLine;
		yourself.
	self assert: item default equals: #unix
]

{ #category : #tests }
ConfigurationItemTest >> testNumber [
	| item |
	item := NumberConfigurationItem new
		propertyName: #headerLevel;
		default: 3;
		yourself.
	self assert: item default equals: 3
]

{ #category : #tests }
ConfigurationItemTest >> testSettingDefaultAbsolutePath [
	| item |
	item := PathConfigurationItem new
		propertyName: #title;
		default: '/build';
		yourself.
	self assert: item default equals: '/build'
]

{ #category : #tests }
ConfigurationItemTest >> testSettingDefaultBoolean [
	| item |
	item := BooleanConfigurationItem new
		propertyName: #separateOutputFiles;
		default: 'false';
		yourself.
	self assert: item default equals: 'false'
]

{ #category : #tests }
ConfigurationItemTest >> testSettingDefaultComplexPath [
	| item |
	item := PathConfigurationItem new
		propertyName: #title;
		default: 'path/to/build';
		yourself.
	self assert: item default equals: 'path/to/build'
]

{ #category : #tests }
ConfigurationItemTest >> testSettingDefaultFile [
	| item |
	item := FileConfigurationItem new
		propertyName: #mainDocument;
		default: 'myfile.pillar';
		yourself.
	self assert: item default equals: 'myfile.pillar'
]

{ #category : #tests }
ConfigurationItemTest >> testSettingDefaultPath [
	| item |
	item := PathConfigurationItem new
		propertyName: #title;
		default: 'build';
		yourself.
	self assert: item default equals: 'build'
]

{ #category : #tests }
ConfigurationItemTest >> testSettingDefaultString [
	| item |
	item := StringConfigurationItem new
		propertyName: #title;
		default: 'my super cool book';
		yourself.
	self assert: item default equals: 'my super cool book' 
]
