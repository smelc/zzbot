# Database

-------------------------
-      builder          -
-------------------------
- ID:Int   primary key  -
- name:String           -
-------------------------

-------------------------
-      build            -
-------------------------
- ID:Int   primary key  -
- builder_id:Int        - -> refers to a builder's primary key
- start:Date            -
- end:Date              -
- status:String         -
-------------------------

-------------------------
-      steps            -
-------------------------
- ID:Int   primary key  -
- build_id:Int          - -> refers to a build's primary key
- description:String    -
- stdout:String         -
- stderr:String         -
- status:String         -
-------------------------
