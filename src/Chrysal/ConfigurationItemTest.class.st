Class {
	#name : #ConfigurationItemTest,
	#superclass : #TestCase,
	#category : #Chrysal
}

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
ConfigurationItemTest >> testSettingDefaultBoolean [
	| item |
	item := StringConfigurationItem new
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
