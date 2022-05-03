library(resources)

resources = ~ localhost()
resources = ~ !localhost()

resources = ~ host(hostname == "localhost")
resources = ~ host(hostname != "localhost")
resources = ~ host(hostname %in% c("localhost", "127.0.0.1", Sys.info()[["nodename"]]))

resources = ~ fork()
resources = ~ !fork()

resources = ~ gpu()
resources = ~ !gpu()

resources = ~ rng()
resources = ~ !rng()
resources = ~ rng(kind = "L'Ecuyer-CMRG")

resources = ~ r(version >= 4.0)

resources = ~ memory(max = 8*GiB)

resources = ~ runtime(max = 00:30:00)
resources = ~ runtime(max = 30*minutes)

resources = ~ license(name = "matlab")

resources = ~ capabilities(which = jpeg || png)
resources = ~ capabilities(which = profmem)

resources = ~ directory("/shared/data/")
resources = ~ directory("/shared/data/", writable = TRUE)
resources = ~ file("/shared/data/foo.txt")
resources = ~ file("/shared/data/foo.txt")

resources = ~ package(future && listenv)
