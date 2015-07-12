package nntpserver

import "strconv"
import "strings"
import "net/textproto"

func articleIDOrNumber(id string) (int64,bool) {
	for len(id)>1 && id[0]=='0' {
		id = id[1:]
	}
	i,e := strconv.ParseInt(id,10,64)
	return i,e==nil
}

func ArticleIDOrNumber(id string) (int64,bool) {
	return articleIDOrNumber(id)
}

func Downlimit(a,b int64) int64{
	if a<b { return b }
	return a
}

func Uplimit(a,b int64) int64{
	if a>b { return b }
	return a
}

func splitgroups(grps string) []string{
	if grps=="" { return []string{} }
	return strings.Split(grps,",")
}

func GetGroups(t textproto.MIMEHeader) (r []string){
	gg,_ := t["Newsgroups"] // nil if not exist
	for _,g := range gg {
		r = append(r,splitgroups(g)...)
	}
	for i,s := range r {
		r[i] = strings.Trim(s," \t\r\n")
	}
	return r
}