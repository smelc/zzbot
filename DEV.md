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
-      step             -
-------------------------
- ID:Int primary key NN -
- build_id:Int NN       - -> refers to a build's primary key
- description:String NN -
- start:Date NN         -
- end:String            -
- stdout:String         -
- stderr:String         -
- status:String         -
-------------------------
