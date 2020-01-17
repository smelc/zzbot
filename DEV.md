# Database

NN=NOT NULL

---------------------------------
-      build                    -
---------------------------------
- ID:Int primary key NN         -
- builder:String primary key NN -
- start:Date NN                 -
- end:Date                      -
- status:String                 -
---------------------------------

-------------------------
-      steps            -
-------------------------
- ID:Int primary key NN -
- build_id:Int NN       - -> refers to a build's primary key
- description:String NN -
- stdout:String NN      -
- stderr:String NN      -
- status:String NN      -
-------------------------
