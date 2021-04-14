
directory <- file.path(tempdir(), "test_form")
formInfo <- setupFormDirectory(xFormId = 588520, formDirectory = directory, token = "42acc1bfe58f9c924bdedc680ab2011622f2bcb5")

xFormFilePath <- file.path(directory, "mold/xForm.xml")
xForm <- XML::xmlTreeParse(xFormFilePath, useInternalNodes = T, encoding = "UTF-8")

xmlMoldFilePath <- file.path(directory, "mold/form_mold.xml")
xmlMold <- XML::xmlTreeParse(xmlMoldFilePath, useInternalNodes = T, encoding = "UTF-8")

csvMoldFilePath <- file.path(directory, "mold/form_mold.csv")
csvMold <- read.csv(csvMoldFilePath)

## Tester structure des fichiers
## Teste hash type du XForm et des mold


## Get instances

instanceRegistry <- getInstanceRegistry(xFormId = 588520, token = "42acc1bfe58f9c924bdedc680ab2011622f2bcb5")
instanceRegistry <- getEncryptedInstances(instanceRegistry, writeToPath = file.path(directory, "instances/encrypted"))

privateKey <- openssl:: base64_encode(openssl::read_key(file = "inst/utils/certificate.pem"))
instanceRegistry <- decryptInstances(instanceRegistry, privateKey, writeToPath = file.path(directory, "instances/decrypted"))
dt <- aggregateInstances(instanceRegistry, formInfo)
