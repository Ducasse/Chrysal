Class {
	#name : #NewLineConfigurationItem,
	#superclass : #ConfigurationItem,
	#instVars : [
		'defaultDomainObject',
		'defaultKey'
	],
	#category : #Chrysal
}

{ #category : #example }
NewLineConfigurationItem class >> example [
	<sampleInstance>
	
	^ NewLineConfigurationItem new
		propertyName: 'newline';
		defaultIsUnix
]

{ #category : #unused }
NewLineConfigurationItem >> defaultDomainValue [
	^ defaultDomainObject 
]

{ #category : #accessing }
NewLineConfigurationItem >> defaultDomainValueString [
	^ self mapperFromUserToDomainRepresentations at: self defaultKey
]

{ #category : #'public api' }
NewLineConfigurationItem >> defaultIsUnix [
	defaultKey := #unix.
	defaultDomainObject := self mapperFromUserToDomain at: #unix
]

{ #category : #accessing }
NewLineConfigurationItem >> defaultKey [
	^ defaultKey ifNil: [ defaultKey := #unix ]
]

{ #category : #'mapping logic' }
NewLineConfigurationItem >> mapperFromUserToDomain [
	
	| fromUserToDomainDictionary|
	fromUserToDomainDictionary := self mapperFromUserToDomainRepresentations copy. 
	fromUserToDomainDictionary keysAndValuesDo: [ :k :v | 
		fromUserToDomainDictionary at: k put: (fromUserToDomainDictionary at: k put: (Compiler evaluate: v))
		].
 	^ fromUserToDomainDictionary
]

{ #category : #'mapping logic' }
NewLineConfigurationItem >> mapperFromUserToDomainRepresentations [

	 ^ (Dictionary
			newFrom:
				{#cr -> 'String cr' .
				#lf -> 'UnixPlatform new lineEnding'.
				#crlf -> 'Win32Platform new lineEnding'.
				#unix -> 'UnixPlatform new lineEnding'.
				#mac -> 'UnixPlatform new lineEnding'.
				#dos -> 'Win32Platform new lineEnding'})
]

{ #category : #unused }
NewLineConfigurationItem >> possibleValuesString [
	^ #(#cr #lf  #crlf  #unix  #mac  #dos)
]
