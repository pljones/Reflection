package info.drealm.s3pi

object Test extends App {
    val exampleResourceType = new ResourceType(24)
    val exampleResource = WrapperDealer.createNewResource(exampleResourceType)
    println("WrapperDealer.createNewResource(new ResourceType(24)): " + exampleResource)

    val demoResourceType = new ResourceType(1234)
    val demoResource = WrapperDealer.createNewResource(demoResourceType)
    println("WrapperDealer.createNewResource(new ResourceType(1234)): " + demoResource)

    val defaultResourceType = new ResourceType(241)
    val defaultResource = WrapperDealer.createNewResource(defaultResourceType)
    println("WrapperDealer.createNewResource(new ResourceType(241)): " + defaultResource)
}