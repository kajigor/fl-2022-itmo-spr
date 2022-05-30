plugins {
    java
    application
    antlr
}


group = "org.itmo.formal_language"
version = "1.0-SNAPSHOT"

tasks.withType<Jar> {
    duplicatesStrategy = DuplicatesStrategy.EXCLUDE

    // Otherwise you'll get a "No main manifest attribute" error
    manifest {
        attributes["Main-Class"] = "ru.itmo.formal_language.BParser"
    }

    // To add all of the dependencies otherwise a "NoClassDefFoundError" error
    from(sourceSets.main.get().output)

    dependsOn(configurations.runtimeClasspath)
    from({
        configurations.runtimeClasspath.get().filter { it.name.endsWith("jar") }.map { zipTree(it) }
    })
}

repositories {
    mavenCentral()
}

dependencies {
    // https://mvnrepository.com/artifact/guru.nidi/graphviz-java
    implementation("guru.nidi:graphviz-java:0.18.1")
    testImplementation(platform("org.junit:junit-bom:5.8.0"))
    testImplementation("org.junit.jupiter:junit-jupiter")
    // https://mvnrepository.com/artifact/commons-cli/commons-cli
    implementation("commons-cli:commons-cli:1.4")


    antlr("org.antlr:antlr4:4.9.3")
    implementation("org.antlr:antlr4-runtime:4.9.3")
}

tasks.generateGrammarSource {
    arguments.add("-Werror")
    arguments.add("-long-messages")
}

tasks.withType<JavaCompile> { dependsOn(tasks.generateGrammarSource) }

//dependencies {
//    antlr "org.antlr:antlr4:4.9.3"
//    implementation group: 'commons-cli', name: 'commons-cli', version: '1.4'
//
//    testImplementation 'org.assertj:assertj-core:3.20.2'
//    testImplementation 'org.junit.jupiter:junit-jupiter-api:5.7.0'
//    testImplementation 'org.junit.jupiter:junit-jupiter-params:5.7.0'
//    testRuntimeOnly 'org.junit.jupiter:junit-jupiter-engine:5.7.0'
//}

tasks.test {
    useJUnitPlatform()
}
