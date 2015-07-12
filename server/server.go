/*
 * The MIT License (MIT)
 * 
 * Copyright (c) 2015 Simon Schmidt
 * Copyright (c) 2012-2014  Dustin Sallings <dustin@spy.net>
 * 
 * Permission is hereby granted, free of charge, to any person obtaining a copy
 * of this software and associated documentation files (the "Software"), to deal
 * in the Software without restriction, including without limitation the rights
 * to use, copy, modify, merge, publish, distribute, sublicense, and/or sell
 * copies of the Software, and to permit persons to whom the Software is
 * furnished to do so, subject to the following conditions:
 * 
 * The above copyright notice and this permission notice shall be included in
 * all copies or substantial portions of the Software.
 * 
 * THE SOFTWARE IS PROVIDED "AS IS", WITHOUT WARRANTY OF ANY KIND, EXPRESS OR
 * IMPLIED, INCLUDING BUT NOT LIMITED TO THE WARRANTIES OF MERCHANTABILITY,
 * FITNESS FOR A PARTICULAR PURPOSE AND NONINFRINGEMENT. IN NO EVENT SHALL THE
 * AUTHORS OR COPYRIGHT HOLDERS BE LIABLE FOR ANY CLAIM, DAMAGES OR OTHER
 * LIABILITY, WHETHER IN AN ACTION OF CONTRACT, TORT OR OTHERWISE, ARISING FROM,
 * OUT OF OR IN CONNECTION WITH THE SOFTWARE OR THE USE OR OTHER DEALINGS IN THE
 * SOFTWARE.
 *
 * <http://www.opensource.org/licenses/mit-license.php>
 *
 *
 * RFC Snippets inside some comments: Copyright (C) The Internet Society (2006).
 */


// Package nntpserver provides everything you need for your own NNTP server.
package nntpserver

import (
	"fmt"
	"io"
	"log"
	"math"
	"net"
	"net/textproto"
	"strconv"
	"strings"
	"time"

	"github.com/maxymania/go-nntp"
)

// An NNTPError is a coded NNTP error message.
type NNTPError struct {
	Code int
	Msg  string
}

// ErrNoSuchGroup is returned for a request for a group that can't be found.
var ErrNoSuchGroup = &NNTPError{411, "No such newsgroup"}

// ErrNoSuchGroup is returned for a request that requires a current
// group when none has been selected.
var ErrNoGroupSelected = &NNTPError{412, "No newsgroup selected"}

// ErrInvalidMessageID is returned when a message is requested that can't be found.
var ErrInvalidMessageID = &NNTPError{430, "No article with that message-id"}

// ErrInvalidArticleNumber is returned when an article is requested that can't be found.
var ErrInvalidArticleNumber = &NNTPError{423, "No article with that number"}

// ErrNoCurrentArticle is returned when a command is executed that
// requires a current article when one has not been selected.
var ErrNoCurrentArticle = &NNTPError{420, "Current article number is invalid"}

// ErrNoNextArticle is returned when LAST or NEXT reaches the end of its iteration
var ErrNoNextArticle = &NNTPError{421, "No next article to retrieve"}

// ErrNoPreviousArticle is returned when LAST or NEXT reaches the end of its iteration
var ErrNoPreviousArticle = &NNTPError{422, "No previous article to retrieve"}

// ErrUnknownCommand is returned for unknown comands.
var ErrUnknownCommand = &NNTPError{500, "Unknown command"}

// ErrSyntax is returned when a command can't be parsed.
var ErrSyntax = &NNTPError{501, "not supported, or syntax error"}

// ErrPostingNotPermitted is returned as the response to an attempt to
// post an article where posting is not permitted.
var ErrPostingNotPermitted = &NNTPError{440, "Posting not permitted"}

// ErrPostingFailed is returned when an attempt to post an article fails.
var ErrPostingFailed = &NNTPError{441, "posting failed"}

// ErrNotWanted is returned when an attempt to ihave an article is
// rejected due the server not wanting the article.
var ErrNotWanted = &NNTPError{435, "Article not wanted"}

// ErrIHaveNopPossible is the same as ErrIHaveFailed, except the text.
// ErrIHaveNopPossible is returned when an attempt to ihave an article fails
// BEFORE the transfer of the article.
var ErrIHaveNotPossible = &NNTPError{436, "Transfer not possible; try again later"}

// ErrIHaveFailed is returned when an attempt to ihave an article fails
// AFTER the transfer of the article.
var ErrIHaveFailed = &NNTPError{436, "Transfer failed; try again later"}

// ErrIHaveRejected is returned when an attempt to ihave an article is
// rejected due the server not wanting the article.
var ErrIHaveRejected = &NNTPError{437, "Transfer rejected; do not retry"}

// ErrAuthRequired is returned to indicate authentication is required
// to proceed.
var ErrAuthRequired = &NNTPError{450, "authorization required"}

// ErrAuthRejected is returned for invalid authentication.
var ErrAuthRejected = &NNTPError{452, "authorization rejected"}

// ErrNotAuthenticated is returned when a command is issued that requires
// authentication, but authentication was not provided.
var ErrNotAuthenticated = &NNTPError{480, "authentication required"}

// Handler is a low-level protocol handler
type Handler func(args []string, s *session, c *textproto.Conn) error

// A NumberedArticle provides local sequence numbers to articles When
// listing articles in a group.
type NumberedArticle struct {
	Num     int64
	Article *nntp.Article
}

// The Backend that provides the things and does the stuff.
type Backend interface {
	// ListGroups(max int) ([]*nntp.Group, error)
	ListGroups(max int) (<- chan *nntp.Group, error)
	GetGroup(name string) (*nntp.Group, error)
	// DONE: Add a way for Article Downloading without group select
	// if not to implement DO: return nil, ErrNoGroupSelected
	GetArticleWithNoGroup(id string) (*nntp.Article, error)
	GetArticle(group *nntp.Group, id string) (*nntp.Article, error)
	// old: GetArticles(group *nntp.Group, from, to int64) ([]NumberedArticle, error)
	// channels are more suitable for large scale
	GetArticles(group *nntp.Group, from, to int64) (<- chan NumberedArticle, error)
	Authorized() bool
	// Authenticate and optionally swap out the backend for this session.
	// You may return nil to continue using the same backend.
	Authenticate(user, pass string) (Backend, error)
	AllowPost() bool
	Post(article *nntp.Article) error
}

// An optional Interface Backend-objects may provide.
//
// This interface provides functions used in the IHAVE command.
// If this interface is not provided by a backend, the server falls back to
// similar functions from the required core interface.
type BackendIHave interface {
	// This method is like the Post(article *nntp.Article)-method except that it is executed
	// on the IHAVE command instead of on the POST command. IHave is required to return
	// ErrIHaveFailed or ErrIHaveRejected instead of the ErrPostingFailed error (otherwise
	// the server will missbehave)
	//
	// If BackendIHave is not provided, the server will use the Post-method with
	// any ErrPostingFailed-result being replaced by ErrIHaveFailed automatically.
	IHave(article *nntp.Article) error

	// This method will tell the server frontent, and thus, the client, wether the server should
	// accept the Article or not.
	// If the article is wanted and should be transfered, nil should be returned.
	// If it is clear, IHAVE would reject, ErrNotWanted should be returned.
	// If it is clear, IHAVE would fail, ErrIHaveNotPossible should be returned.
	// 
	// If BackendIHave is not provided, the server will use the method
	// GetArticleWithNoGroup-method to determine.
	IHaveWantArticle(id string) error
}

type IdGenerator interface {
	GenID() string
}

type session struct {
	server      *Server
	backend     Backend
	idGenerator IdGenerator
	group       *nntp.Group
	number		int64
	beIhave     BackendIHave
}
func (s *session) setBackend(backend Backend){
	s.backend   = backend
	s.beIhave,_ = backend.(BackendIHave)
}

// The Server handle.
type Server struct {
	// Handlers are dispatched by command name.
	Handlers map[string]Handler
	// The backend (your code) that provides data
	Backend Backend
	// The Id Generator (your code) that provides Article IDs
	IdGenerator IdGenerator
	// The currently selected group.
	group *nntp.Group
}

// NewServer builds a new server handle request to a backend.
func NewServer(backend Backend,idGenerator IdGenerator) *Server {
	rv := Server{
		Handlers:    make(map[string]Handler),
		Backend:     backend,
		IdGenerator: idGenerator,
	}
	rv.Handlers[""] = handleDefault
	rv.Handlers["quit"] = handleQuit
	rv.Handlers["group"] = handleGroup
	rv.Handlers["list"] = handleList
	rv.Handlers["head"] = handleHead
	rv.Handlers["body"] = handleBody
	rv.Handlers["article"] = handleArticle
	rv.Handlers["post"] = handlePost
	rv.Handlers["ihave"] = handleIHave
	rv.Handlers["capabilities"] = handleCap
	rv.Handlers["mode"] = handleMode
	rv.Handlers["authinfo"] = handleAuthInfo
	rv.Handlers["newgroups"] = handleNewGroups
	rv.Handlers["over"] = handleOver
	rv.Handlers["xover"] = handleOver
	rv.Handlers["hdr"] = handleHdr
	rv.Handlers["xhdr"] = handleHdr
	rv.Handlers["listgroup"] = handleListgroup
	rv.Handlers["last"] = handleLast
	rv.Handlers["next"] = handleNext
	rv.Handlers["stat"] = handleStat
	rv.Handlers["help"] = handleHelp
	rv.Handlers["date"] = handleDate
	return &rv
}

func (e *NNTPError) Error() string {
	return fmt.Sprintf("%d %s", e.Code, e.Msg)
}

func (s *session) dispatchCommand(cmd string, args []string,
	c *textproto.Conn) (err error) {

	handler, found := s.server.Handlers[strings.ToLower(cmd)]
	if !found {
		handler, found = s.server.Handlers[""]
		if !found {
			panic("No default handler.")
		}
	}
	return handler(args, s, c)
}

// Process an NNTP session.
func (s *Server) Process(tc net.Conn) {
	defer tc.Close()
	c := textproto.NewConn(tc)
	
	var backend Backend
	if s.Backend!=nil {
		backend = s.Backend
	} else {
		panic("no backend set")
	}

	sess := &session{
		server:      s,
		idGenerator: s.IdGenerator,
		group:       nil,
		number:      0,
	}
	sess.setBackend(backend)

	c.PrintfLine("200 Hello!")
	for {
		l, err := c.ReadLine()
		if err != nil {
			log.Printf("Error reading from client, dropping conn: %v", err)
			return
		}
		cmd := strings.Split(l, " ")
		log.Printf("Got cmd:  %+v", cmd)
		args := []string{}
		if len(cmd) > 1 {
			args = cmd[1:]
		}
		err = sess.dispatchCommand(cmd[0], args, c)
		if err != nil {
			_, isNNTPError := err.(*NNTPError)
			switch {
			case err == io.EOF:
				// Drop this connection silently. They hung up
				return
			case isNNTPError:
				c.PrintfLine(err.Error())
			default:
				log.Printf("Error dispatching command, dropping conn: %v",
					err)
				return
			}
		}
	}
}

func parseRange(spec string) (low, high int64) {
	if spec == "" {
		return 0, math.MaxInt64
	}
	parts := strings.Split(spec, "-")
	if len(parts) == 0 {
		return 0, math.MaxInt64
	}
	if len(parts) == 1 {
		h, err := strconv.ParseInt(parts[0], 10, 64)
		if err != nil {
			h = math.MaxInt64
		}
		return 0, h
	}
	l, _ := strconv.ParseInt(parts[0], 10, 64)
	h, err := strconv.ParseInt(parts[1], 10, 64)
	if err != nil {
		h = math.MaxInt64
	}
	return l, h
}

/*
   Indicating capability: READER

   Syntax
     LISTGROUP [group [range]]

   Responses
     211 number low high group     Article numbers follow (multi-line)
     411                           No such newsgroup
     412                           No newsgroup selected [1]

   Parameters
     group     Name of newsgroup
     range     Range of articles to report
     number    Estimated number of articles in the group
     low       Reported low water mark
     high      Reported high water mark

   [1] The 412 response can only occur if no group has been specified.
*/
func handleListgroup(args []string, s *session, c *textproto.Conn) error {
	grp := s.group
	arg0 := ""
	arg1 := ""
	if len(args)>0 {arg0 = args[0]}
	if len(args)>1 {arg1 = args[1]}
	if arg0!="" {
		ok := false
		if grp!=nil { ok = grp.Name==arg0 }
		if grp==nil || !ok {
			var err error
			grp, err = s.backend.GetGroup(args[0])
			if err != nil {
				return err
			}
		}
	}
	if grp==nil { return ErrNoGroupSelected }

	from, to := parseRange(arg1)
	articles, err := s.backend.GetArticles(grp, from, to)
	if err != nil {
		return err
	}

	c.PrintfLine("211 %d %d %d %s",grp.Count,grp.Low,grp.High,grp.Name)
	dw := c.DotWriter()
	defer dw.Close()
	for a := range articles {
		fmt.Fprintf(dw, "%d\n", a.Num)
	}
	return nil
}



/*
Indicating capability: OVER

   Syntax
     OVER message-id
     OVER range
     OVER

   Responses

   First form (message-id specified)
     224    Overview information follows (multi-line)
     430    No article with that message-id

   Second form (range specified)
     224    Overview information follows (multi-line)
     412    No newsgroup selected
     423    No articles in that range

   Third form (current article number used)
     224    Overview information follows (multi-line)
     412    No newsgroup selected
     420    Current article number is invalid

   Parameters
     range         Number(s) of articles
     message-id    Message-id of article
*/
/*
   "0" or article number (see below)
   Subject header content
   From header content
   Date header content
   Message-ID header content
   References header content
   :bytes metadata item
   :lines metadata item
*/
func handleOver(args []string, s *session, c *textproto.Conn) error {
	arg0 := ""
	if len(args)>0 {arg0 = args[0]}
	single,nogroup := analiyzeArticleID(arg0)
	if s.group == nil && !nogroup {
		return ErrNoGroupSelected
	}
	if single {
		var a *nntp.Article
		var e error
		if nogroup{
			a,e = s.backend.GetArticleWithNoGroup(arg0)
		}else{
			a,e = s.backend.GetArticle(s.group,arg0)
		}
		if e!=nil { return e }
		dw := c.DotWriter()
		defer dw.Close()
		fmt.Fprintf(dw, "%d\t%s\t%s\t%s\t%s\t%s\t%d\t%d\n", 0,
			a.Header.Get("Subject"),
			a.Header.Get("From"),
			a.Header.Get("Date"),
			a.Header.Get("Message-Id"),
			a.Header.Get("References"),
			a.Bytes, a.Lines)
		return nil
	}
	from, to := parseRange(arg0)
	articles, err := s.backend.GetArticles(s.group, from, to)
	if err != nil {
		return err
	}
	c.PrintfLine("224 here it comes")
	dw := c.DotWriter()
	defer dw.Close()
	for a := range articles {
		fmt.Fprintf(dw, "%d\t%s\t%s\t%s\t%s\t%s\t%d\t%d\n", a.Num,
			a.Article.Header.Get("Subject"),
			a.Article.Header.Get("From"),
			a.Article.Header.Get("Date"),
			a.Article.Header.Get("Message-Id"),
			a.Article.Header.Get("References"),
			a.Article.Bytes, a.Article.Lines)
	}
	return nil
}

/*
   Indicating capability: OVER

   Syntax
     LIST OVERVIEW.FMT

   Responses
     215    Information follows (multi-line)
*/
func handleListOverviewFmt(dw io.Writer, c *textproto.Conn) error {
	err := c.PrintfLine("215 Information follows")
	if err != nil {
		return err
	}
	// This is NOT a performance critical function
	_, err = fmt.Fprintln(dw, "Subject:"); if err!=nil { return err }
	_, err = fmt.Fprintln(dw, "From:"); if err!=nil { return err }
	_, err = fmt.Fprintln(dw, "Date:"); if err!=nil { return err }
	_, err = fmt.Fprintln(dw, "Message-ID:"); if err!=nil { return err }
	_, err = fmt.Fprintln(dw, "References:"); if err!=nil { return err }
	_, err = fmt.Fprintln(dw, ":bytes"); if err!=nil { return err }
	_, err = fmt.Fprintln(dw, ":lines"); if err!=nil { return err }
    
	return nil
}

/*

   Indicating capability: HDR

   Syntax
     HDR field message-id
     HDR field range
     HDR field

   Responses

   First form (message-id specified)
     225    Headers follow (multi-line)
     430    No article with that message-id

   Second form (range specified)
     225    Headers follow (multi-line)
     412    No newsgroup selected
     423    No articles in that range

   Third form (current article number used)
     225    Headers follow (multi-line)
     412    No newsgroup selected
     420    Current article number is invalid

   Parameters
     field         Name of field
     range         Number(s) of articles
     message-id    Message-id of article
*/
func handleHdr(args []string, s *session, c *textproto.Conn) error {
	arg0 := ""
	arg1 := ""
	if len(args)>0 {arg0 = args[0]}
	if len(args)>1 {arg1 = args[1]}
	single,nogroup := analiyzeArticleID(arg1)
	if s.group == nil && !nogroup {
		return ErrNoGroupSelected
	}
	if single {
		var a *nntp.Article
		var e error
		if nogroup{
			a,e = s.backend.GetArticleWithNoGroup(arg1)
		}else{
			a,e = s.backend.GetArticle(s.group,arg1)
		}
		if e!=nil { return e }
		dw := c.DotWriter()
		defer dw.Close()
		switch arg0{
		case ":bytes":
			fmt.Fprintf(dw, "%d\t%d\n", 0, a.Bytes)
		case ":lines":
			fmt.Fprintf(dw, "%d\t%d\n", 0, a.Lines)
		default:
			fmt.Fprintf(dw, "%d\t%s\n", 0, a.Header.Get(arg0))
		}
		return nil
	}

	from, to := parseRange(arg1)
	articles, err := s.backend.GetArticles(s.group, from, to)
	if err != nil {
		return err
	}
	c.PrintfLine("224 here it comes")
	dw := c.DotWriter()
	defer dw.Close()
	switch arg0{
	case ":bytes":
		for a := range articles {
			fmt.Fprintf(dw, "%d\t%d\n", a.Num,a.Article.Bytes)
		}
	case ":lines":
		for a := range articles {
			fmt.Fprintf(dw, "%d\t%d\n", a.Num,a.Article.Lines)
		}
	default:
		for a := range articles {
			fmt.Fprintf(dw, "%d\t%s\n", a.Num,
				a.Article.Header.Get(arg0))
		}
	}
	return nil
}

/*
   Indicating capability: HDR

   Syntax
     LIST HEADERS [MSGID|RANGE]

   Responses
     215    Field list follows (multi-line)

   Parameters
     MSGID    Requests list for access by message-id
     RANGE    Requests list for access by range
*/
func handleListHeaders(dw io.Writer, c *textproto.Conn) error {
	err := c.PrintfLine("215 Field list follows")
	if err != nil {
		return err
	}
	// This is NOT a performance critical function
	_, err = fmt.Fprintln(dw, ":"); if err!=nil { return err }
	_, err = fmt.Fprintln(dw, ":bytes"); if err!=nil { return err }
	_, err = fmt.Fprintln(dw, ":lines"); if err!=nil { return err }
	return nil
}


/*
   Indicating capability: LIST

   Syntax
     LIST [keyword [wildmat|argument]]

   Responses
     215    Information follows (multi-line)

   Parameters
     keyword     Information requested [1]
     argument    Specific to keyword
     wildmat     Groups of interest

   [1] If no keyword is provided, it defaults to ACTIVE.

   Example of LIST with the ACTIVE keyword and wildmat:

      [C] LIST ACTIVE misc.*
      [S] 215 list of newsgroups follows
      [S] misc.test 3002322 3000234 y
      [S] .

*/
func handleList(args []string, s *session, c *textproto.Conn) error {
	ltype := "active"
	var wildmat *WildMat
	wildmat = nil
	if len(args) > 0 {
		ltype = strings.ToLower(args[0])
	}
	
	if ltype == "overview.fmt" {
		dw := c.DotWriter()
		defer dw.Close()
		return handleListOverviewFmt(dw, c)
	} else if ltype=="headers" {
		dw := c.DotWriter()
		defer dw.Close()
		return handleListHeaders(dw, c)
	}
	
	if len(args) > 1 {
		wildmat = ParseWildMat(args[1])
		e := wildmat.Compile()
		if e!=nil { return ErrSyntax }
	}
	
	groups, err := s.backend.ListGroups(-1)
	if err != nil {
		return err
	}
	c.PrintfLine("215 list of newsgroups follows")
	dw := c.DotWriter()
	defer dw.Close()
	for g := range groups {
		if wildmat!=nil {
			if !wildmat.Match(g.Name) { continue }
		}
		switch ltype {
		case "active":
			fmt.Fprintf(dw, "%s %d %d %v\r\n",
				g.Name, g.High, g.Low, g.Posting)
		case "newsgroups":
			fmt.Fprintf(dw, "%s %s\r\n", g.Name, g.Description)
		}
	}
	
	return nil
}

/*
   Indicating capability: READER

   Syntax
     NEWGROUPS date time [GMT]

   Responses
     231    List of new newsgroups follows (multi-line)

   Parameters
     date    Date in yymmdd or yyyymmdd format
     time    Time in hhmmss format

   >>Description<<

   This command returns a list of newsgroups created on the server since
   the specified date and time. The results are in the same format as
   the LIST ACTIVE command (see Section 7.6.3).  However, they MAY
   include groups not available on the server (and so not returned by
   LIST ACTIVE) and MAY omit groups for which the creation date is not
   available.
*/
func handleNewGroups(args []string, s *session, c *textproto.Conn) error {
	c.PrintfLine("231 list of newsgroups follows")
	c.PrintfLine(".")
	return nil
}

func handleDefault(args []string, s *session, c *textproto.Conn) error {
	return ErrUnknownCommand
}

func handleQuit(args []string, s *session, c *textproto.Conn) error {
	c.PrintfLine("205 bye")
	return io.EOF
}

/*
   Indicating capability: READER

   Syntax
     GROUP group

   Responses
     211 number low high group     Group successfully selected
     411                           No such newsgroup

   Parameters
     group     Name of newsgroup
     number    Estimated number of articles in the group
     low       Reported low water mark
     high      Reported high water mark

*/
func handleGroup(args []string, s *session, c *textproto.Conn) error {
	if len(args) < 1 {
		return ErrNoSuchGroup
	}

	group, err := s.backend.GetGroup(args[0])
	if err != nil {
		return err
	}

	s.group = group
	s.number = -1

	c.PrintfLine("211 %d %d %d %s",
		group.Count, group.Low, group.High, group.Name)
	return nil
}

/*
   Indicating capability: READER

   Syntax
     LAST

   Responses
     223 n message-id    Article found
     412                 No newsgroup selected
     420                 Current article number is invalid
     422                 No previous article in this group

   Parameters
     n             Article number
     message-id    Article message-id

Moves the current article pointer to the previous article.
*/
func handleLast(args []string, s *session, c *textproto.Conn) error {
	if s.group == nil {
		return ErrNoGroupSelected
	}
	if s.number<0 {
		s.number = s.group.High+1
	}
	for s.group.Low<=s.number {
		s.number--
		a,_ := s.backend.GetArticle(s.group,fmt.Sprint(s.number))
		if a!=nil {
			c.PrintfLine("223 %d %s",s.number,a.MessageID())
			return nil
		}
	}
	return ErrNoPreviousArticle
}

/*
   Indicating capability: READER

   Syntax
     NEXT

   Responses
     223 n message-id    Article found
     412                 No newsgroup selected
     420                 Current article number is invalid
     421                 No next article in this group

   Parameters
     n             Article number
     message-id    Article message-id

Moves the current article pointer to the next article.
*/
func handleNext(args []string, s *session, c *textproto.Conn) error {
	if s.group == nil {
		return ErrNoGroupSelected
	}
	if s.number<0 {
		s.number = s.group.Low-1
	}
	for s.number<=s.group.High {
		s.number++
		a,_ := s.backend.GetArticle(s.group,fmt.Sprint(s.number))		
		if a!=nil {
			c.PrintfLine("223 %d %s",s.number,a.MessageID())
			return nil
		}
	}
	return ErrNoNextArticle
}

/*
   Syntax
     STAT message-id
     STAT number
     STAT

   Responses

   First form (message-id specified)
     223 0|n message-id    Article exists
     430                   No article with that message-id

   Second form (article number specified)
     223 n message-id      Article exists
     412                   No newsgroup selected
     423                   No article with that number

   Third form (current article number used)
     223 n message-id      Article exists
     412                   No newsgroup selected
     420                   Current article number is invalid

   Parameters
     number        Requested article number
     n             Returned article number
     message-id    Article message-id

If a article number is passed, the server should set the "current article pointer" to it.
*/
func handleStat(args []string, s *session, c *textproto.Conn) error {
	article, err := s.getArticle(args)
	if err != nil {
		return err
	}
	if len(args)>0 {
		n,ok := articleIDOrNumber(args[0])
		if ok { s.number = n }
	}
	c.PrintfLine("223 1 %s", article.MessageID())
	return nil
}


// internal
func (s *session) getArticle(args []string) (*nntp.Article, error) {
	if len(args)==0{
		if s.group == nil {
			return nil, ErrNoGroupSelected
		}
		if s.number<0 || s.number>s.group.High {
			return nil,ErrNoCurrentArticle
		}
		return s.backend.GetArticle(s.group, fmt.Sprint(s.number))
	}
	if s.group == nil {
		return s.backend.GetArticleWithNoGroup(args[0])
		// return nil, ErrNoGroupSelected
	}
	return s.backend.GetArticle(s.group, args[0])
}

/*
   Syntax
     HEAD message-id
     HEAD number
     HEAD


   First form (message-id specified)
     221 0|n message-id    Headers follow (multi-line)
     430                   No article with that message-id

   Second form (article number specified)
     221 n message-id      Headers follow (multi-line)
     412                   No newsgroup selected
     423                   No article with that number

   Third form (current article number used)
     221 n message-id      Headers follow (multi-line)
     412                   No newsgroup selected
     420                   Current article number is invalid
*/
func handleHead(args []string, s *session, c *textproto.Conn) error {
	article, err := s.getArticle(args)
	if err != nil {
		return err
	}
	c.PrintfLine("221 1 %s", article.MessageID())
	dw := c.DotWriter()
	defer dw.Close()
	for k, v := range article.Header {
		for _,vv := range v {
			fmt.Fprintf(dw, "%s: %s\r\n", k, vv)
		}
	}
	return nil
}

/*
   Syntax
     BODY message-id
     BODY number
     BODY

   Responses

   First form (message-id specified)
     222 0|n message-id    Body follows (multi-line)
     430                   No article with that message-id

   Second form (article number specified)
     222 n message-id      Body follows (multi-line)
     412                   No newsgroup selected
     423                   No article with that number

   Third form (current article number used)
     222 n message-id      Body follows (multi-line)
     412                   No newsgroup selected
     420                   Current article number is invalid

   Parameters
     number        Requested article number
     n             Returned article number
     message-id    Article message-id
*/
func handleBody(args []string, s *session, c *textproto.Conn) error {
	article, err := s.getArticle(args)
	if err != nil {
		return err
	}
	c.PrintfLine("222 1 %s", article.MessageID())
	dw := c.DotWriter()
	defer dw.Close()
	_, err = io.Copy(dw, article.Body)
	return err
}

/*
   Syntax
     ARTICLE message-id
     ARTICLE number
     ARTICLE

   Responses

   First form (message-id specified)
     220 0|n message-id    Article follows (multi-line)
     430                   No article with that message-id

   Second form (article number specified)
     220 n message-id      Article follows (multi-line)
     412                   No newsgroup selected
     423                   No article with that number

   Third form (current article number used)
     220 n message-id      Article follows (multi-line)
     412                   No newsgroup selected
     420                   Current article number is invalid

   Parameters
     number        Requested article number
     n             Returned article number
     message-id    Article message-id
*/
func handleArticle(args []string, s *session, c *textproto.Conn) error {
	article, err := s.getArticle(args)
	if err != nil {
		return err
	}
	c.PrintfLine("220 1 %s", article.MessageID())
	dw := c.DotWriter()
	defer dw.Close()

	for k, v := range article.Header {
		for _,vv := range v {
			fmt.Fprintf(dw, "%s: %s\r\n", k, vv)
		}
	}

	fmt.Fprintln(dw, "")

	_, err = io.Copy(dw, article.Body)
	return err
}

/*
   Indicating capability: POST

   This command MUST NOT be pipelined.

   Syntax
     POST

   Responses

   Initial responses
     340    Send article to be posted
     440    Posting not permitted

   Subsequent responses
     240    Article received OK
     441    Posting failed
*/
func handlePost(args []string, s *session, c *textproto.Conn) error {
	if !s.backend.AllowPost() {
		return ErrPostingNotPermitted
	}

	c.PrintfLine("340 Go ahead")
	var err error
	var article nntp.Article
	article.Header, err = c.ReadMIMEHeader()
	if err != nil {
		return ErrPostingFailed
	}
	{
		msgID := article.Header.Get("Message-Id")
		if msgID=="" {
			article.Header.Set("Message-Id",s.idGenerator.GenID())
		}
	}
	article.Body = c.DotReader()
	err = s.backend.Post(&article)
	if err != nil {
		return err
	}
	c.PrintfLine("240 article received OK")
	return nil
}

/*

   Indicating capability: IHAVE

   This command MUST NOT be pipelined.

   Syntax
     IHAVE message-id

   Responses

   Initial responses
     335    Send article to be transferred
     435    Article not wanted
     436    Transfer not possible; try again later

   Subsequent responses
     235    Article transferred OK
     436    Transfer failed; try again later
     437    Transfer rejected; do not retry

   Parameters
     message-id    Article message-id

*/
func handleIHave(args []string, s *session, c *textproto.Conn) error {
	if !s.backend.AllowPost() {
		return ErrNotWanted
	}
	var article *nntp.Article
	var err error

	if s.beIhave!=nil { goto way_use_beIhave }

	// See if we have it.
	article, err = s.backend.GetArticleWithNoGroup(args[0])
	if article != nil {
		return ErrNotWanted
	}

	c.PrintfLine("335 send it")
	article = &nntp.Article{}
	article.Header, err = c.ReadMIMEHeader()
	if err != nil {
		return ErrIHaveFailed
	}
	article.Body = c.DotReader()
	err = s.backend.Post(article)
	if err != nil {
		if err==ErrPostingFailed { err = ErrIHaveFailed }
		return err
	}
	return c.PrintfLine("235 article received OK")
	
	way_use_beIhave:
	
	// See if we have it.
	err = s.beIhave.IHaveWantArticle(args[0])
	if err != nil {
		return err
	}

	c.PrintfLine("335 send it")
	article = &nntp.Article{}
	article.Header, err = c.ReadMIMEHeader()
	if err != nil {
		return ErrIHaveFailed
	}
	article.Body = c.DotReader()
	err = s.beIhave.IHave(article)
	if err != nil {
		return err
	}
	return c.PrintfLine("235 article received OK")
}


/*
   Syntax
     HELP

   Responses
     100    Help text follows (multi-line)
*/
func handleHelp(args []string, s *session, c *textproto.Conn) error {
	c.PrintfLine("100 Help text follows (multi-line)")
	c.PrintfLine(".")
	return nil
}

/*
   Indicating capability: READER

   Syntax
     DATE

   Responses
     111 yyyymmddhhmmss    Server date and time
*/
func handleDate(args []string, s *session, c *textproto.Conn) error {
	t := time.Now()
	Y,M,D := t.Date()
	h,m,z := t.Clock()
	c.PrintfLine("111 %04d%02d%02d%02d%02d%02d",Y,int(M),D,h,m,z)
	return nil
}

func handleCap(args []string, s *session, c *textproto.Conn) error {
	c.PrintfLine("101 Capability list:")
	dw := c.DotWriter()
	defer dw.Close()

	fmt.Fprintf(dw, "VERSION 2\n")
	fmt.Fprintf(dw, "READER\n")
	if s.backend.AllowPost() {
		fmt.Fprintf(dw, "POST\n")
		fmt.Fprintf(dw, "IHAVE\n")
	}
	fmt.Fprintf(dw, "OVER\n")
	fmt.Fprintf(dw, "XOVER\n")
	fmt.Fprintf(dw, "HDR\n")
	fmt.Fprintf(dw, "XHDR\n")
	fmt.Fprintf(dw, "LIST ACTIVE NEWSGROUPS HEADER OVERVIEW.FMT\n")
	return nil
}

func handleMode(args []string, s *session, c *textproto.Conn) error {
	if s.backend.AllowPost() {
		c.PrintfLine("200 Posting allowed")
	} else {
		c.PrintfLine("201 Posting prohibited")
	}
	return nil
}

/*
 Documented outside RFC 3977 --> RFC 4643

   Example of successful AUTHINFO USER:

      [C] AUTHINFO USER wilma
      [S] 281 Authentication accepted

   Example of successful AUTHINFO USER/PASS:

      [C] AUTHINFO USER fred
      [S] 381 Enter passphrase
      [C] AUTHINFO PASS flintstone
      [S] 281 Authentication accepted

   Example of AUTHINFO USER/PASS requiring a security layer:

      [C] AUTHINFO USER fred@stonecanyon.example.com
      [S] 483 Encryption or stronger authentication required

   Example of failed AUTHINFO USER/PASS:

      [C] AUTHINFO USER barney
      [S] 381 Enter passphrase
      [C] AUTHINFO PASS flintstone
      [S] 481 Authentication failed

   Example of AUTHINFO PASS before AUTHINFO USER:

      [C] AUTHINFO PASS flintstone
      [S] 482 Authentication commands issued out of sequence
*/
func handleAuthInfo(args []string, s *session, c *textproto.Conn) error {
	if len(args) < 2 {
		return ErrSyntax
	}
	if strings.ToLower(args[0]) != "user" {
		if strings.ToLower(args[0]) == "pass" {
			return c.PrintfLine("482 Authentication commands issued out of sequence")
		}
		return ErrSyntax
	}

	if s.backend.Authorized() {
		return c.PrintfLine("250 authenticated")
	}

	c.PrintfLine("350 Continue")
	a, err := c.ReadLine()
	parts := strings.SplitN(a, " ", 3)
	if strings.ToLower(parts[0]) != "authinfo" || strings.ToLower(parts[1]) != "pass" {
		return ErrSyntax
	}
	b, err := s.backend.Authenticate(args[1], parts[2])
	if err == nil {
		c.PrintfLine("250 authenticated")
		if b != nil {
			s.setBackend(b)
		}
	}
	return err
}


