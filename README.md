# AGYNAMIX Gartenverein

# Twitter Bootstrap

Bootstrap v3.0 is included.

# MongoDB

This app uses MongoDB. Therefore, you will need to either have it installed locally, or use one of
the cloud providers and configure it in your props file. See config.MongoConfig for more info.

# Building

This app uses sbt 0.13.0. To build for the first time, run:

    bash$ sbt
    > ~;container:start; container:reload /

That will start the app and automatically reload it whenever sources are modified. It will be running
on http://localhost:8080


