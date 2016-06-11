
-module(bc_web_files).

-export([routes_file/0, 
		 views_dir/0,
		 view_file/1,
		 static_dir/0, 
		 assets_dir/0]).

routes_file() ->
	filename:join([code:priv_dir(bc_web), "bc_web.routes"]).

views_dir() ->
	filename:join([code:priv_dir(bc_web), "views"]).

view_file(ViewFile) ->
	filename:join([views_dir(), ViewFile]).

static_dir() ->
	filename:join([code:priv_dir(bc_web), "static"]).
	
assets_dir() ->
	filename:join([static_dir(), "assets"]).
