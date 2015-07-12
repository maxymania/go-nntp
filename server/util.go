package nntpserver

import "strconv"
import "strings"
import "net/textproto"

var headerCorrection = map[string]string{
	"Message-Id":"Message-ID",
}
func correctHeader(h string) string {
	h2,ok := headerCorrection[h]
	if ok { return h2 }
	return h
}

func analiyzeArticleID(id string) (single bool,nogroup bool) {
	if len(id)==0 { return false,false }
	if id[0]=='<' { return true,true }
	if strings.Index(id,"-")<0 { return true,false }
	return false,false
}

func articleIDOrNumber(id string) (int64,bool) {
	for len(id)>1 && id[0]=='0' {
		id = id[1:]
	}
	i,e := strconv.ParseInt(id,10,64)
	return i,e==nil
}

// Distinguishes between an Article-ID and an Article Number
// using the syntax of the id-String.
func ArticleIDOrNumber(id string) (int64,bool) {
	return articleIDOrNumber(id)
}

// Utility function:
// Assures the return value does not deceed the minimum value.
// 
// value = Downlimit(value,minimum_value)
func Downlimit(a,b int64) int64{
	if a<b { return b }
	return a
}

// Utility function:
// Assures the return value does not exceed the maximum value.
// 
// value = Uplimit(value,maximum_value)
func Uplimit(a,b int64) int64{
	if a>b { return b }
	return a
}

func splitgroups(grps string) []string{
	if grps=="" { return []string{} }
	return strings.Split(grps,",")
}

// Utility function:
// Retrieves all Newsgroups headers, splits ","-concatenated lists,
// trimms all names.
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
