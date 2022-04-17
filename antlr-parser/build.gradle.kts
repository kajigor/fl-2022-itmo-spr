plugins {
    java
    antlr
    application
}

group = "me.fang"
version = "1.0-SNAPSHOT"

val main = "me.fang.parser.App"

application {
    mainClass.set(main)
}

repositories {
    mavenCentral()
}

dependencies {
    antlr("org.antlr:antlr4:4.10.1")
    testImplementation("org.junit.jupiter:junit-jupiter-api:5.8.2")
    testRuntimeOnly("org.junit.jupiter:junit-jupiter-engine:5.8.2")
}

tasks.getByName<Test>("test") {
    useJUnitPlatform()
}
