package nntpserver

import "strconv"

func articleIDOrNumber(id string) (int64,bool) {
	for len(id)>1 && id[0]=='0' {
		id = id[1:]
	}
	i,e := strconv.ParseInt(id,10,64)
	return i,e==nil
}
