
-module(bc_web_files).

-export([routes_file/0, 
		 views_dir/0,
		 view_file/1,
		 static_dir/0, 
		 assets_dir/0]).

-spec routes_file() -> string().
routes_file() ->
	filename:join([code:priv_dir(bc_web), "bc_web.routes"]).

-spec views_dir() -> string().
views_dir() ->
	filename:join([code:priv_dir(bc_web), "views"]).

-spec view_file(ViewFile :: string()) -> string().
view_file(ViewFile) ->
	filename:join([views_dir(), ViewFile]).

-spec static_dir() -> string().
static_dir() ->
	filename:join([code:priv_dir(bc_web), "static"]).

-spec assets_dir() -> string().
assets_dir() ->
	filename:join([static_dir(), "assets"]).
