'(("version" . "3")
  ("services"
   ("opensearch-node1"
    ("volumes"
     .
     #("/var/lib/opensearch:/usr/share/opensearch/data"))
    ("ulimits"
     ("nofile" ("soft" . 65536) ("hard" . 65536))
     ("memlock" ("soft" . -1) ("hard" . -1)))
    ("ports"
     .
     #("127.0.0.1:9200:9200" "127.0.0.1:9600:9600"))
    ("image" . "opensearchproject/opensearch:1.2.4")
    ("environment"
     .
     #("cluster.name=opensearch-cluster"
       "node.name=opensearch-node1"
       "discovery.seed_hosts=opensearch-node1"
       "cluster.initial_master_nodes=opensearch-node1"
       "bootstrap.memory_lock=true"
       "OPENSEARCH_JAVA_OPTS=-Xms512m -Xmx512m"))
    ("container_name" . "opensearch-node1"))
   ("opensearch-dashboards"
    ("ports" . #("127.0.0.1:5601:5601"))
    ("image"
     .
     "opensearchproject/opensearch-dashboards:1.2.0")
    ("expose" . #("5601"))
    ("environment"
     ("OPENSEARCH_HOSTS"
      .
      "[\"https://opensearch-node1:9200\"]"))
    ("container_name" . "opensearch-dashboards"))))
