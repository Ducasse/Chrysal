Class {
	#name : #ConfigurationDescriptionForXX,
	#superclass : #ConfigurationDescription,
	#category : #Chrysal
}

{ #category : #accessing }
ConfigurationDescriptionForXX >> initialize [
	items := {self separatedOutput.
	self newLine.
	self outputDirectory.
	self mainDocument}
]

{ #category : #accessing }
ConfigurationDescriptionForXX >> mainDocument [
	^ FileConfigurationItem new
		propertyName: 'mainDocument';
		default: 'book'
]

{ #category : #accessing }
ConfigurationDescriptionForXX >> newLine [
	^ NewLineConfigurationItem new
		propertyName: 'newline';
		defaultIsUnix
]

{ #category : #accessing }
ConfigurationDescriptionForXX >> outputDirectory [
	^ FolderConfigurationItem new
		propertyName: 'outputDirectory';
		default: 'build'
]

{ #category : #accessing }
ConfigurationDescriptionForXX >> separatedOutput [
	^ BooleanConfigurationItem new
		propertyName: 'separateOutputFiles';
		beTrueAsDefault
]
