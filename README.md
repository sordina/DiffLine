
# DiffLine

Show differences between successive lines...

Usage:

		diffline [-s | --slashes (default) ] [-r | --repeats] [-v | --vertical] [-h | --help]

		> find temp -type f | head

		temp/-name/serf/local.snapshot
		temp/-node/serf/local.snapshot
		temp/.DS_Store
		temp/a
		temp/a.js
		temp/a.out
		temp/a.txt
		temp/adrian.mid
		temp/adsf.sql
		temp/am/www.anthonymaydwell.com/_derived/home_cmp_classic110_vbtn.gif

		> find temp -type f | head | diffline -r

		temp/-name/serf/local.snapshot
           ode/serf/local.snapshot
         .DS_Store
         a
          .js
           out
           txt
          drian.mid
           sf.sql

		> find temp -type f | head | diffline -s

		temp/-name/serf/local.snapshot
        /-node/serf/local.snapshot
        /.DS_Store
        /a
        /a.js
        /a.out
        /a.txt
        /adrian.mid
        /adsf.sql

		> find temp -type f | head | diffline -v

		temp/-name/serf/local.snapshot
				/-node/serf/local.snapshot
				/.DS_Store
				/a
				/a.js
				/a.out
				/a.txt
				/adrian.mid
				/adsf.sql
