package info.drealm.s3pi

object Test extends App {

    val exampleResourceType = new ResourceType(24)
    val exampleResource = WrapperDealer.createNewResource(exampleResourceType)
    
    println(exampleResource)

    val defaultResourceType = new ResourceType(241)
    val defaultResource = WrapperDealer.createNewResource(defaultResourceType)
    
    println(defaultResource)
}