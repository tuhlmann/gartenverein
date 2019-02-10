# AGYNAMIX Gartenverein

For instructions to build and run the application please see the end of this file.

Gartenverein is an attempt to create a hosted application to manage small garden communities in Germany.

Garden communities are managed like associations. Invoices need to be sent, water and power meter readings have to be collected and stored.

This software manages garden tenants, readings, documents, a shared calendar and more.

## After logging in you are greeted with a Dashboard like overview
![Gartenverein Overview](/doc/garden-overview.png)

## A list of documents
![Documents](/doc/garden-documents.png)

## Generated documents.

The system supports ODF document templates and can generate bulk letters for all or a group of users:
![Generated Documents](/doc/garden-generated-documents.png)

## Viewing the members of a garden community
![Gartenverein Members](/doc/garden-members.png)

## Uploading and Editing ODF document templates
![ODF Document Templates](/doc/garden-document-template.png)

Gartenverein was discontinued due to lack of resources.
I post it here if maybe someone find some ideas in the code useful for his or her own projects.

# Building and Running Gartenverein

Gartenverein is a Scala and [Lift](https://liftweb.net) project.

# Twitter Bootstrap

Bootstrap v3.0 is included.

# MongoDB

This app uses MongoDB. Therefore, you will need to either have it installed locally, or use one of
the cloud providers and configure it in your props file. See config.MongoConfig for more info.

# Configuring

Please check `src/main/resources/props/default.props` and replace the placeholders with real values.

# Building

This app uses sbt 0.13.0. To build for the first time, run:

    bash$ sbt
    > ~;container:start; container:reload /

That will start the app and automatically reload it whenever sources are modified. It will be running
on http://localhost:8080

