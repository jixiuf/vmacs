--- === SuperGenPass ===
---
--- hammerspoon version of  SuperGenPass
--- https://chriszarate.github.io/supergenpass/
---
---
---  you can use this plugin like this
--- local superGenPass=hs.loadSpoon("SuperGenPass")
--- superGenPass.saveGeneratedPasswordToPasteboard=true --auto save generated password to pasteboard
--- superGenPass.remberMasterPassword=false
--- superGenPass.autoHideWindowAfterPasswordGenerated=false
--- superGenPass:bindHotkeys({toggle={{"cmd", "alt"}, "h"}})
--- superGenPass:start()

local obj = { __gc = true }

--obj.__index = obj

-- Metadata
obj.name = "SuperGenPass"
obj.version = "1.0"
obj.author = "JiXiufeng <jixiuf@qq.com>"
obj.homepage = "https://github.com/jixiuf/dotfiles/tree/master/mac/hammerspoon/Spoons/SuperGenPass.spoon"
obj.license = "GNU General Public License version 2 - http://www.gnu.org/licenses/gpl-2.0.html"

-- customizable variables
obj.saveGeneratedPasswordToPasteboard = false -- auto save generated password to pasteboard
obj.autoHideWindowAfterPasswordGenerated = true -- after  saveGeneratedPasswordToPasteboard ,auto hide window
--for Safari user: You must enable the 'Allow JavaScript from Apple Events' option in Safari's Develop menu to use 'do JavaScript'.
--for Chrome user: You must enable 'Allow JavaScript from Apple Events' by going to the menu bar, View > Developer > Allow JavaScript from Apple Events
obj.autoComplete = true -- auto complete the generated password for safari and chrome
obj.remberMasterPassword = false -- remember master password or not
obj.showMenubar = true -- show menubar or not
obj.defaultAppDomainMap = {
   ["net.nutstore.NutstoreJavaBE"] = "jianguoyun.com",
   ["com.apple.iBooksX"] = "apple.com",
   ["com.apple.iTunes"] = "apple.com",

}

-- obj.defaultAppDomainMap={}


-- do not change variables
obj.hotkey = nil
obj.menuBarItem = nil
obj.webview = nil
obj.masterPassword = ""
obj.prevFocusedWindow=nil

function obj:init()
   return self
end
--- SuperGenPass:start()
--- Method
--- Starts SuperGenPass
---
--- Parameters:
---  * None
---
--- Returns:
---  * The SuperGenPass object

function obj:start()
   if obj.showMenubar then
      self.menuBarItem = hs.menubar.new()
      self.menuBarItem:setClickCallback(self.clicked)
      self.menuBarItem:setTooltip("SuperGenPass \nhttps://chriszarate.github.io/supergenpass/mobile/")
      -- https://www.compart.com/en/unicode/U+0050
      -- https://unicode-search.net/unicode-namesearch.pl?term=P
      self.menuBarItem:setTitle("\u{1D40F}") -- init the menu bar
   end

   if (self.hotkey) then
      self.hotkey:enable()
   end
   return self
end

--- SuperGenPass:stop()
--- Method
--- Stops SuperGenPass
---
--- Parameters:
---  * None
---
--- Returns:
---  * The SuperGenPass object
---

function obj:stop()
    if (self.hotkey) then
        self.hotkey:disable()
    end
   return self
end

--- SuperGenPass:bindHotkeys(mapping)
--- Method
--- Binds hotkeys for SuperGenPass
---
--- Parameters:
---  * mapping - A table containing hotkey modifier/key details for the following (optional) items:
---   * toggle - This will cause SuperGenPass's UI to be shown or hidden depending on its current state
---
--- Returns:
---  * The SuperGenPass object
function obj:bindHotkeys(mapping)
    if (self.hotkey) then
        self.hotkey:delete()
    end
    if mapping["toggle"] ~= nil then
        local toggleMods = mapping["toggle"][1]
        local toggleKey = mapping["toggle"][2]
        self.hotkey = hs.hotkey.new(toggleMods, toggleKey, function() self:clicked() end)
    end

    return self
end



local function serializeTable(val, name, skipnewlines, depth)
    skipnewlines = skipnewlines or false
    depth = depth or 0

    local tmp = string.rep(" ", depth)

    if name then tmp = tmp .. name .. " = " end

    if type(val) == "table" then
        tmp = tmp .. "{" .. (not skipnewlines and "\n" or "")

        for k, v in pairs(val) do
            tmp =  tmp .. serializeTable(v, k, skipnewlines, depth + 1) .. "," .. (not skipnewlines and "\n" or "")
        end

        tmp = tmp .. string.rep(" ", depth) .. "}"
    elseif type(val) == "number" then
        tmp = tmp .. tostring(val)
    elseif type(val) == "string" then
        tmp = tmp .. string.format("%q", val)
    elseif type(val) == "boolean" then
        tmp = tmp .. (val and "true" or "false")
    else
        tmp = tmp .. "\"[inserializeable datatype:" .. type(val) .. "]\""
    end

    return tmp
end

function obj.getAddressDefault()
   local win = hs.window.frontmostWindow()
   if win ==nil then
      return ""
   end
   local app = win:application()
   if app:bundleID() =="com.apple.finder" and win:role()== "AXScrollArea" then -- 如果是桌面
      return ""
   elseif  app:bundleID() =="com.apple.Safari" then
      return obj.getAddressOfSafari()
   elseif  app:bundleID() =="com.google.Chrome" then
      return obj.getAddressOfChrome()
   elseif  app:bundleID() =="org.mozilla.firefox" then
      return obj.getAddressOfFirefox()
   else
      defaultAppDomain=obj.defaultAppDomainMap[app:bundleID() ]
      if defaultAppDomain then
         return defaultAppDomain
      else
         return ""
      end
   end
   return ""
end
function obj.getAddressOfFirefox()
   return hs.pasteboard.getContents()
end

function obj.getAddressOfSafari()
   scpt=[[
tell application "System Events"
	if not (exists process "Safari") then
		""
	else
		tell application "Safari" to URL of document 1
	end if
end tell
]]
   succ,address,desc=   hs.osascript.applescript(scpt)
   if succ then
      return address
   else
      return ""
   end
end

function obj.getAddressOfChrome()
   scpt=[[
tell application "System Events"
	if not (exists process "Chrome") then
		""
	else
    tell application "Google Chrome"
        get URL of active tab of first window
    end tell
	end if
end tell
]]
   succ,address,desc=   hs.osascript.applescript(scpt)
   if succ then
      return address
   else
      return ""
   end
end





function obj.windowClosing(cmd, wv, opt)
   if cmd == "closing" then
      if obj.webview~=nil then
         -- obj.webview:sendToBack()
         obj.webview:hide()
         -- obj.webview:delete()
         obj.webview=nil
         if obj.prevFocusedWindow ~=nil then
            obj.prevFocusedWindow:focus()
         end
      end
   end
end
function obj.windowFocusChange(cmd, wv, opt)
   if cmd == "focusChange" then
      if obj.webview~=nil and not opt  then
         -- obj.webview:sendToBack()
         obj.webview:hide()
         -- obj.webview:delete()
         obj.webview=nil
         if obj.prevFocusedWindow ~=nil then
            obj.prevFocusedWindow:focus()
         end

      end
   elseif cmd == "closing" then
      if obj.webview~=nil then
         -- obj.webview:sendToBack()
         obj.webview:hide()
         -- obj.webview:delete()
         obj.webview=nil
         if obj.prevFocusedWindow ~=nil then
            obj.prevFocusedWindow:focus()
         end
      end
   end
end
function obj.clicked()
   -- hs.application.launchOrFocusByBundleID("org.hammerspoon.Hammerspoon")
   if obj.webview ~=nil then
      -- obj.webview:sendToBack()
      -- obj.webview:hide()
      if obj.prevFocusedWindow ~=nil then
         obj.prevFocusedWindow:focus()
      end
      obj.webview:hide()
      -- obj.webview:delete()
      obj.webview=nil
      return
   end
   obj.prevFocusedWindow = hs.window.focusedWindow()


   local htmlContent = [[
<html>
<head>
<title>SuperGenPass</title>
		<style type="text/css">

			body {
				margin: 0;
				padding: 0;
				background: #ddd;
				color: #000;
				font-family: Helvetica, Arial, sans-serif;
				font-size: 0.9em;
			}

			header {
				display: block;
				padding: 0.75em;
				background: #444;
				background: -webkit-gradient(linear, left top, left bottom, from(#666), to(#111));
				background: -moz-linear-gradient(top, #666, #111);
				filter: progid:DXImageTransform.Microsoft.gradient(startColorstr='#666', endColorstr='#111');
				border-bottom: solid 1px #234;
			}

			header h1 {
				overflow: hidden;
				margin: 1px 0;
				font-size: 1.4em;
				font-weight: bold;
				text-align: center;
			}

			header h1 a { color: #fff; text-decoration: none; }

			section, footer {
				display: block;
				margin: 0.75em;
			}

			fieldset {
				margin: 0;
				padding: 0.75em;
				background: #fff;
				font-size: 1em;
				border: solid 1px #999;
				border-bottom-width: 0;
				line-height: 1.3em;
			}

			fieldset.First {
				border-top-left-radius: 0.5em; -moz-border-radius-topleft: 0.5em; -webkit-border-top-left-radius: 0.5em;
				border-top-right-radius: 0.5em; -moz-border-radius-topright: 0.5em; -webkit-border-top-right-radius: 0.5em;
			}

			fieldset.Last {
				border-bottom-width: 1px;
				border-bottom-left-radius: 0.5em; -moz-border-radius-bottomleft: 0.5em; -webkit-border-bottom-left-radius: 0.5em;
				border-bottom-right-radius: 0.5em; -moz-border-radius-bottomright: 0.5em; -webkit-border-bottom-right-radius: 0.5em;
			}

			fieldset label {
				display: block;
				float: left;
				margin-right: 0.75em;
				font-size: 1em;
				font-weight: bold;
				line-height: 1.3em;
			}

			fieldset input {
				margin: 0;
				padding: 0;
				color: #456;
				font-size: 1em;
				line-height: 1.3em;
			}

			fieldset div.Field { overflow: hidden; }

			fieldset div.Field input {
				width: 100%;
				background: none;
				border: none;
				outline: none; -webkit-appearance: none;
			}

			div#Generate a {

				display: block;
				margin: 0.75em 0;
				padding: 0.5em;
				vertical-align: baseline;

				background: #aaa;
				background: -webkit-gradient(linear, left top, left bottom, from(#ccc), to(#999));
				background: -moz-linear-gradient(top, #ccc, #999);
				filter: progid:DXImageTransform.Microsoft.gradient(startColorstr='#ccc', endColorstr='#999');

				color: #333;
				font-size: 1.2em;
				font-weight: bold;
				text-align: center;
				text-decoration: none;

				cursor: pointer;
				border: solid 2px #777;
				border-radius: 0.5em; -webkit-border-radius: 0.5em; -moz-border-radius: 0.5em;

			}

			div#Generate a:hover {
				text-decoration: none;
				background: #bbb;
				background: -webkit-gradient(linear, left top, left bottom, from(#ddd), to(#aaa));
				background: -moz-linear-gradient(top, #ddd, #aaa);
				filter: progid:DXImageTransform.Microsoft.gradient(startColorstr='#ddd', endColorstr='#aaa');
			}

			div#Generate a:active {
				top: 1px;
				position: relative;
				background: -webkit-gradient(linear, left top, left bottom, from(#999), to(#ccc));
				background: -moz-linear-gradient(top, #999, #ccc);
				filter: progid:DXImageTransform.Microsoft.gradient(startColorstr='#999', endColorstr='#ccc');
			}

			div#Result {
                display:none;
				padding: 0.5em;
				background: #ada;
				font-size: 1.2em;
				font-weight: bold;
				text-align: center;
				border: solid 2px #696;
				border-radius: 0.5em; -moz-border-radius: 0.5em; -webkit-border-radius: 0.5em;
			}


		</style>
		<script type="text/javascript">

			function b64_md5(p) {
				p=utf8_en(p);
				return binl2b64(core_md5(str2binl(p),p.length*8));
			}

			function hex_md5(p) {
				p=utf8_en(p);
				return binl2hex(core_md5(str2binl(p),p.length*8));
			}

			function binl2b64(binarray) {
				var tab='ABCDEFGHIJKLMNOPQRSTUVWXYZabcdefghijklmnopqrstuvwxyz012345678998';
				var str='';
				for(var i=0; i<binarray.length*4; i+=3) {
					var triplet=(((binarray[i>>2]>>8*(i%4))&0xFF)<<16)|(((binarray[i+1>>2]>>8*((i+1)%4))&0xFF)<<8)|((binarray[i+2>>2]>>8*((i+2)%4))&0xFF);
					for(var j=0; j<4; j++) {
						str+=tab.charAt((triplet>>6*(3-j))&0x3F);
					}
				}
				return str;
			}

			function binl2hex(binarray) {
				var hex_tab='0123456789abcdef';
				var str='';
				for(var i=0; i<binarray.length*4; i++) {
					str+=hex_tab.charAt((binarray[i>>2]>>((i%4)*8+4))&0xF)+hex_tab.charAt((binarray[i>>2]>>((i%4)*8))&0xF);
				}
				return str;
			}

			function core_md5(x,len){
				x[len>>5]|=0x80<<((len)%32); x[(((len+64)>>>9)<<4)+14]=len;
				var a=1732584193; var b=-271733879; var c=-1732584194; var d=271733878;
				for(var i=0;i<x.length;i+=16){
					var olda=a; var oldb=b; var oldc=c; var oldd=d;
					a=md5_ff(a,b,c,d,x[i+0],7,-680876936); d=md5_ff(d,a,b,c,x[i+1],12,-389564586); c=md5_ff(c,d,a,b,x[i+2],17,606105819); b=md5_ff(b,c,d,a,x[i+3],22,-1044525330);
					a=md5_ff(a,b,c,d,x[i+4],7,-176418897); d=md5_ff(d,a,b,c,x[i+5],12,1200080426); c=md5_ff(c,d,a,b,x[i+6],17,-1473231341); b=md5_ff(b,c,d,a,x[i+7],22,-45705983);
					a=md5_ff(a,b,c,d,x[i+8],7,1770035416); d=md5_ff(d,a,b,c,x[i+9],12,-1958414417); c=md5_ff(c,d,a,b,x[i+10],17,-42063); b=md5_ff(b,c,d,a,x[i+11],22,-1990404162);
					a=md5_ff(a,b,c,d,x[i+12],7,1804603682); d=md5_ff(d,a,b,c,x[i+13],12,-40341101); c=md5_ff(c,d,a,b,x[i+14],17,-1502002290); b=md5_ff(b,c,d,a,x[i+15],22,1236535329);
					a=md5_gg(a,b,c,d,x[i+1],5,-165796510); d=md5_gg(d,a,b,c,x[i+6],9,-1069501632); c=md5_gg(c,d,a,b,x[i+11],14,643717713); b=md5_gg(b,c,d,a,x[i+0],20,-373897302);
					a=md5_gg(a,b,c,d,x[i+5],5,-701558691); d=md5_gg(d,a,b,c,x[i+10],9,38016083); c=md5_gg(c,d,a,b,x[i+15],14,-660478335); b=md5_gg(b,c,d,a,x[i+4],20,-405537848);
					a=md5_gg(a,b,c,d,x[i+9],5,568446438); d=md5_gg(d,a,b,c,x[i+14],9,-1019803690); c=md5_gg(c,d,a,b,x[i+3],14,-187363961); b=md5_gg(b,c,d,a,x[i+8],20,1163531501);
					a=md5_gg(a,b,c,d,x[i+13],5,-1444681467); d=md5_gg(d,a,b,c,x[i+2],9,-51403784); c=md5_gg(c,d,a,b,x[i+7],14,1735328473); b=md5_gg(b,c,d,a,x[i+12],20,-1926607734);
					a=md5_hh(a,b,c,d,x[i+5],4,-378558); d=md5_hh(d,a,b,c,x[i+8],11,-2022574463); c=md5_hh(c,d,a,b,x[i+11],16,1839030562); b=md5_hh(b,c,d,a,x[i+14],23,-35309556);
					a=md5_hh(a,b,c,d,x[i+1],4,-1530992060); d=md5_hh(d,a,b,c,x[i+4],11,1272893353); c=md5_hh(c,d,a,b,x[i+7],16,-155497632); b=md5_hh(b,c,d,a,x[i+10],23,-1094730640);
					a=md5_hh(a,b,c,d,x[i+13],4,681279174); d=md5_hh(d,a,b,c,x[i+0],11,-358537222); c=md5_hh(c,d,a,b,x[i+3],16,-722521979); b=md5_hh(b,c,d,a,x[i+6],23,76029189);
					a=md5_hh(a,b,c,d,x[i+9],4,-640364487); d=md5_hh(d,a,b,c,x[i+12],11,-421815835); c=md5_hh(c,d,a,b,x[i+15],16,530742520); b=md5_hh(b,c,d,a,x[i+2],23,-995338651);
					a=md5_ii(a,b,c,d,x[i+0],6,-198630844); d=md5_ii(d,a,b,c,x[i+7],10,1126891415); c=md5_ii(c,d,a,b,x[i+14],15,-1416354905); b=md5_ii(b,c,d,a,x[i+5],21,-57434055);
					a=md5_ii(a,b,c,d,x[i+12],6,1700485571); d=md5_ii(d,a,b,c,x[i+3],10,-1894986606); c=md5_ii(c,d,a,b,x[i+10],15,-1051523); b=md5_ii(b,c,d,a,x[i+1],21,-2054922799);
					a=md5_ii(a,b,c,d,x[i+8],6,1873313359); d=md5_ii(d,a,b,c,x[i+15],10,-30611744); c=md5_ii(c,d,a,b,x[i+6],15,-1560198380); b=md5_ii(b,c,d,a,x[i+13],21,1309151649);
					a=md5_ii(a,b,c,d,x[i+4],6,-145523070); d=md5_ii(d,a,b,c,x[i+11],10,-1120210379); c=md5_ii(c,d,a,b,x[i+2],15,718787259); b=md5_ii(b,c,d,a,x[i+9],21,-343485551);
					a=safe_add(a,olda); b=safe_add(b,oldb); c=safe_add(c,oldc); d=safe_add(d,oldd);
				}
				return Array(a,b,c,d);
			}

			function md5_cmn(q,a,b,x,s,t) { return safe_add(bit_rol(safe_add(safe_add(a,q),safe_add(x,t)),s),b); }
			function md5_ff(a,b,c,d,x,s,t) { return md5_cmn((b&c)|((~b)&d),a,b,x,s,t); }
			function md5_gg(a,b,c,d,x,s,t) { return md5_cmn((b&d)|(c&(~d)),a,b,x,s,t); }
			function md5_hh(a,b,c,d,x,s,t) { return md5_cmn(b^c^d,a,b,x,s,t); }
			function md5_ii(a,b,c,d,x,s,t) { return md5_cmn(c^(b|(~d)),a,b,x,s,t); }
			function safe_add(x,y) { var lsw=(x&0xFFFF)+(y&0xFFFF); var msw=(x>>16)+(y>>16)+(lsw>>16); return (msw<<16)|(lsw&0xFFFF); }
			function bit_rol(num,cnt) { return (num<<cnt)|(num>>>(32-cnt)); }
			function str2binl(str) { var bin=Array(); var mask=(1<<8)-1; for(var i=0;i<str.length*8;i+=8) bin[i>>5]|=(str.charCodeAt(i/8)&mask)<<(i%32); return bin; }
			function utf8_en(str){return unescape(encodeURIComponent(str));}

			function gp2_generate_passwd(Passwd,Len) {
				var i=0;
				while(i<10||!(gp2_check_passwd(Passwd.substring(0,Len)))) {
					Passwd=b64_md5(Passwd);
					i++;
				}
				return Passwd.substring(0,Len);
			}

			function gp2_check_passwd(Passwd) {
				return (Passwd.search(/[a-z]/)===0&&Passwd.search(/[0-9]/)>0&&Passwd.search(/[A-Z]/)>0)?true:false;
			}

			function gp2_validate_length(Len) {
				Len=(parseInt(Len))?parseInt(Len):10;
				if(Len<4) {
					Len=4;
				} else if(Len>24) {
					Len=24;
				}
				return Len;
			}

			function gp2_process_uri(URI,DisableTLD) {

				URI=URI.toLowerCase();
				var HostNameIsolator=new RegExp('^(http|https|ftp|ftps|webdav|gopher|rtsp|irc|nntp|pop|imap|smtp)://([^/:]+)');
				var HostName=URI.match(HostNameIsolator);

				if(HostName&&HostName[2]!=null) {
					HostName=HostName[2];
				} else {
					HostNameIsolator=new RegExp('^([^/:]+)');
					HostName=URI.match(HostNameIsolator);
					HostName=(HostName[1]!=null)?HostName[1]:URI;
				}

				HostNameIsolator=new RegExp('^([0-9]{1,3}\.[0-9]{1,3}\.[0-9]{1,3}\.[0-9]{1,3})$');
				HostName=(HostName.match(HostNameIsolator))?[HostName]:HostName.split('.');

				if(HostName[2]==null||DisableTLD) {
					URI=HostName.join('.');
				} else {
					URI=HostName[HostName.length-2]+'.'+HostName[HostName.length-1];
					var TLDList=['ac.ac','com.ac','edu.ac','gov.ac','net.ac','mil.ac','org.ac','com.ae','net.ae','org.ae','gov.ae','ac.ae','co.ae','sch.ae','pro.ae','com.ai','org.ai','edu.ai','gov.ai','com.ar','net.ar','org.ar','gov.ar','mil.ar','edu.ar','int.ar','co.at','ac.at','or.at','gv.at','priv.at','com.au','gov.au','org.au','edu.au','id.au','oz.au','info.au','net.au','asn.au','csiro.au','telememo.au','conf.au','otc.au','id.au','com.az','net.az','org.az','com.bb','net.bb','org.bb','ac.be','belgie.be','dns.be','fgov.be','com.bh','gov.bh','net.bh','edu.bh','org.bh','com.bm','edu.bm','gov.bm','org.bm','net.bm','adm.br','adv.br','agr.br','am.br','arq.br','art.br','ato.br','bio.br','bmd.br','cim.br','cng.br','cnt.br','com.br','coop.br','ecn.br','edu.br','eng.br','esp.br','etc.br','eti.br','far.br','fm.br','fnd.br','fot.br','fst.br','g12.br','ggf.br','gov.br','imb.br','ind.br','inf.br','jor.br','lel.br','mat.br','med.br','mil.br','mus.br','net.br','nom.br','not.br','ntr.br','odo.br','org.br','ppg.br','pro.br','psc.br','psi.br','qsl.br','rec.br','slg.br','srv.br','tmp.br','trd.br','tur.br','tv.br','vet.br','zlg.br','com.bs','net.bs','org.bs','ab.ca','bc.ca','mb.ca','nb.ca','nf.ca','nl.ca','ns.ca','nt.ca','nu.ca','on.ca','pe.ca','qc.ca','sk.ca','yk.ca','gc.ca','co.ck','net.ck','org.ck','edu.ck','gov.ck','com.cn','edu.cn','gov.cn','net.cn','org.cn','ac.cn','ah.cn','bj.cn','cq.cn','gd.cn','gs.cn','gx.cn','gz.cn','hb.cn','he.cn','hi.cn','hk.cn','hl.cn','hn.cn','jl.cn','js.cn','ln.cn','mo.cn','nm.cn','nx.cn','qh.cn','sc.cn','sn.cn','sh.cn','sx.cn','tj.cn','tw.cn','xj.cn','xz.cn','yn.cn','zj.cn','arts.co','com.co','edu.co','firm.co','gov.co','info.co','int.co','nom.co','mil.co','org.co','rec.co','store.co','web.co','ac.cr','co.cr','ed.cr','fi.cr','go.cr','or.cr','sa.cr','com.cu','net.cu','org.cu','ac.cy','com.cy','gov.cy','net.cy','org.cy','co.dk','art.do','com.do','edu.do','gov.do','gob.do','org.do','mil.do','net.do','sld.do','web.do','com.dz','org.dz','net.dz','gov.dz','edu.dz','ass.dz','pol.dz','art.dz','com.ec','k12.ec','edu.ec','fin.ec','med.ec','gov.ec','mil.ec','org.ec','net.ec','com.ee','pri.ee','fie.ee','org.ee','med.ee','com.eg','edu.eg','eun.eg','gov.eg','net.eg','org.eg','sci.eg','com.er','net.er','org.er','edu.er','mil.er','gov.er','ind.er','com.es','org.es','gob.es','edu.es','nom.es','com.et','gov.et','org.et','edu.et','net.et','biz.et','name.et','info.et','ac.fj','com.fj','gov.fj','id.fj','org.fj','school.fj','com.fk','ac.fk','gov.fk','net.fk','nom.fk','org.fk','asso.fr','nom.fr','barreau.fr','com.fr','prd.fr','presse.fr','tm.fr','aeroport.fr','assedic.fr','avocat.fr','avoues.fr','cci.fr','chambagri.fr','chirurgiens-dentistes.fr','experts-comptables.fr','geometre-expert.fr','gouv.fr','greta.fr','huissier-justice.fr','medecin.fr','notaires.fr','pharmacien.fr','port.fr','veterinaire.fr','com.ge','edu.ge','gov.ge','mil.ge','net.ge','org.ge','pvt.ge','co.gg','org.gg','sch.gg','ac.gg','gov.gg','ltd.gg','ind.gg','net.gg','alderney.gg','guernsey.gg','sark.gg','com.gr','edu.gr','gov.gr','net.gr','org.gr','com.gt','edu.gt','net.gt','gob.gt','org.gt','mil.gt','ind.gt','com.gu','edu.gu','net.gu','org.gu','gov.gu','mil.gu','com.hk','net.hk','org.hk','idv.hk','gov.hk','edu.hk','co.hu','2000.hu','erotika.hu','jogasz.hu','sex.hu','video.hu','info.hu','agrar.hu','film.hu','konyvelo.hu','shop.hu','org.hu','bolt.hu','forum.hu','lakas.hu','suli.hu','priv.hu','casino.hu','games.hu','media.hu','szex.hu','sport.hu','city.hu','hotel.hu','news.hu','tozsde.hu','tm.hu','erotica.hu','ingatlan.hu','reklam.hu','utazas.hu','ac.id','co.id','go.id','mil.id','net.id','or.id','co.il','net.il','org.il','ac.il','gov.il','k12.il','muni.il','idf.il','co.im','net.im','org.im','ac.im','lkd.co.im','gov.im','nic.im','plc.co.im','co.in','net.in','ac.in','ernet.in','gov.in','nic.in','res.in','gen.in','firm.in','mil.in','org.in','ind.in','ac.ir','co.ir','gov.ir','id.ir','net.ir','org.ir','sch.ir','ac.je','co.je','net.je','org.je','gov.je','ind.je','jersey.je','ltd.je','sch.je','com.jo','org.jo','net.jo','gov.jo','edu.jo','mil.jo','ad.jp','ac.jp','co.jp','go.jp','or.jp','ne.jp','gr.jp','ed.jp','lg.jp','net.jp','org.jp','gov.jp','hokkaido.jp','aomori.jp','iwate.jp','miyagi.jp','akita.jp','yamagata.jp','fukushima.jp','ibaraki.jp','tochigi.jp','gunma.jp','saitama.jp','chiba.jp','tokyo.jp','kanagawa.jp','niigata.jp','toyama.jp','ishikawa.jp','fukui.jp','yamanashi.jp','nagano.jp','gifu.jp','shizuoka.jp','aichi.jp','mie.jp','shiga.jp','kyoto.jp','osaka.jp','hyogo.jp','nara.jp','wakayama.jp','tottori.jp','shimane.jp','okayama.jp','hiroshima.jp','yamaguchi.jp','tokushima.jp','kagawa.jp','ehime.jp','kochi.jp','fukuoka.jp','saga.jp','nagasaki.jp','kumamoto.jp','oita.jp','miyazaki.jp','kagoshima.jp','okinawa.jp','sapporo.jp','sendai.jp','yokohama.jp','kawasaki.jp','nagoya.jp','kobe.jp','kitakyushu.jp','utsunomiya.jp','kanazawa.jp','takamatsu.jp','matsuyama.jp','com.kh','net.kh','org.kh','per.kh','edu.kh','gov.kh','mil.kh','ac.kr','co.kr','go.kr','ne.kr','or.kr','pe.kr','re.kr','seoul.kr','kyonggi.kr','com.kw','net.kw','org.kw','edu.kw','gov.kw','com.la','net.la','org.la','com.lb','org.lb','net.lb','edu.lb','gov.lb','mil.lb','com.lc','edu.lc','gov.lc','net.lc','org.lc','com.lv','net.lv','org.lv','edu.lv','gov.lv','mil.lv','id.lv','asn.lv','conf.lv','com.ly','net.ly','org.ly','co.ma','net.ma','org.ma','press.ma','ac.ma','com.mk','com.mm','net.mm','org.mm','edu.mm','gov.mm','com.mn','org.mn','edu.mn','gov.mn','museum.mn','com.mo','net.mo','org.mo','edu.mo','gov.mo','com.mt','net.mt','org.mt','edu.mt','tm.mt','uu.mt','com.mx','net.mx','org.mx','gob.mx','edu.mx','com.my','org.my','gov.my','edu.my','net.my','com.na','org.na','net.na','alt.na','edu.na','cul.na','unam.na','telecom.na','com.nc','net.nc','org.nc','ac.ng','edu.ng','sch.ng','com.ng','gov.ng','org.ng','net.ng','gob.ni','com.ni','net.ni','edu.ni','nom.ni','org.ni','com.np','net.np','org.np','gov.np','edu.np','ac.nz','co.nz','cri.nz','gen.nz','geek.nz','govt.nz','iwi.nz','maori.nz','mil.nz','net.nz','org.nz','school.nz','com.om','co.om','edu.om','ac.om','gov.om','net.om','org.om','mod.om','museum.om','biz.om','pro.om','med.om','com.pa','net.pa','org.pa','edu.pa','ac.pa','gob.pa','sld.pa','edu.pe','gob.pe','nom.pe','mil.pe','org.pe','com.pe','net.pe','com.pg','net.pg','ac.pg','com.ph','net.ph','org.ph','mil.ph','ngo.ph','aid.pl','agro.pl','atm.pl','auto.pl','biz.pl','com.pl','edu.pl','gmina.pl','gsm.pl','info.pl','mail.pl','miasta.pl','media.pl','mil.pl','net.pl','nieruchomosci.pl','nom.pl','org.pl','pc.pl','powiat.pl','priv.pl','realestate.pl','rel.pl','sex.pl','shop.pl','sklep.pl','sos.pl','szkola.pl','targi.pl','tm.pl','tourism.pl','travel.pl','turystyka.pl','com.pk','net.pk','edu.pk','org.pk','fam.pk','biz.pk','web.pk','gov.pk','gob.pk','gok.pk','gon.pk','gop.pk','gos.pk','edu.ps','gov.ps','plo.ps','sec.ps','com.pt','edu.pt','gov.pt','int.pt','net.pt','nome.pt','org.pt','publ.pt','com.py','net.py','org.py','edu.py','com.qa','net.qa','org.qa','edu.qa','gov.qa','asso.re','com.re','nom.re','com.ro','org.ro','tm.ro','nt.ro','nom.ro','info.ro','rec.ro','arts.ro','firm.ro','store.ro','www.ro','com.ru','net.ru','org.ru','gov.ru','pp.ru','com.sa','edu.sa','sch.sa','med.sa','gov.sa','net.sa','org.sa','pub.sa','com.sb','net.sb','org.sb','edu.sb','gov.sb','com.sd','net.sd','org.sd','edu.sd','sch.sd','med.sd','gov.sd','tm.se','press.se','parti.se','brand.se','fh.se','fhsk.se','fhv.se','komforb.se','kommunalforbund.se','komvux.se','lanarb.se','lanbib.se','naturbruksgymn.se','sshn.se','org.se','pp.se','com.sg','net.sg','org.sg','edu.sg','gov.sg','per.sg','com.sh','net.sh','org.sh','edu.sh','gov.sh','mil.sh','gov.st','saotome.st','principe.st','consulado.st','embaixada.st','org.st','edu.st','net.st','com.st','store.st','mil.st','co.st','com.sv','org.sv','edu.sv','gob.sv','red.sv','com.sy','net.sy','org.sy','gov.sy','ac.th','co.th','go.th','net.th','or.th','com.tn','net.tn','org.tn','edunet.tn','gov.tn','ens.tn','fin.tn','nat.tn','ind.tn','info.tn','intl.tn','rnrt.tn','rnu.tn','rns.tn','tourism.tn','com.tr','net.tr','org.tr','edu.tr','gov.tr','mil.tr','bbs.tr','k12.tr','gen.tr','co.tt','com.tt','org.tt','net.tt','biz.tt','info.tt','pro.tt','int.tt','coop.tt','jobs.tt','mobi.tt','travel.tt','museum.tt','aero.tt','name.tt','gov.tt','edu.tt','nic.tt','us.tt','uk.tt','ca.tt','eu.tt','es.tt','fr.tt','it.tt','se.tt','dk.tt','be.tt','de.tt','at.tt','au.tt','co.tv','com.tw','net.tw','org.tw','edu.tw','idv.tw','gov.tw','com.ua','net.ua','org.ua','edu.ua','gov.ua','ac.ug','co.ug','or.ug','go.ug','co.uk','me.uk','org.uk','edu.uk','ltd.uk','plc.uk','net.uk','sch.uk','nic.uk','ac.uk','gov.uk','nhs.uk','police.uk','mod.uk','dni.us','fed.us','com.uy','edu.uy','net.uy','org.uy','gub.uy','mil.uy','com.ve','net.ve','org.ve','co.ve','edu.ve','gov.ve','mil.ve','arts.ve','bib.ve','firm.ve','info.ve','int.ve','nom.ve','rec.ve','store.ve','tec.ve','web.ve','co.vi','net.vi','org.vi','com.vn','biz.vn','edu.vn','gov.vn','net.vn','org.vn','int.vn','ac.vn','pro.vn','info.vn','health.vn','name.vn','com.vu','edu.vu','net.vu','org.vu','de.vu','ch.vu','fr.vu','com.ws','net.ws','org.ws','gov.ws','edu.ws','ac.yu','co.yu','edu.yu','org.yu','com.ye','net.ye','org.ye','gov.ye','edu.ye','mil.ye','ac.za','alt.za','bourse.za','city.za','co.za','edu.za','gov.za','law.za','mil.za','net.za','ngo.za','nom.za','org.za','school.za','tm.za','web.za','co.zw','ac.zw','org.zw','gov.zw','eu.org','au.com','br.com','cn.com','de.com','de.net','eu.com','gb.com','gb.net','hu.com','no.com','qc.com','ru.com','sa.com','se.com','uk.com','uk.net','us.com','uy.com','za.com','dk.org','tel.no','fax.nr','mob.nr','mobil.nr','mobile.nr','tel.nr','tlf.nr','e164.arpa'];
					for(var i=0; i<TLDList.length; i++) {
						if(URI==TLDList[i]) {
							URI=HostName[HostName.length-3]+'.'+URI;
							break;
						}
					}
				}

				return URI;

			}

			function SGPLocal() {

				var Passwd=document.getElementById('Passwd').value;
				var DisableTLD=(document.getElementById('DisableTLD').checked)?true:false;
				var Domain=document.getElementById('Domain').value;
				var Len=gp2_validate_length(document.getElementById('Len').value);

				document.getElementById('Len').value=Len;

				if(Domain) {
					Domain=gp2_process_uri(Domain,DisableTLD);
					document.getElementById('Domain').value=Domain;
					if(Passwd) {
                        prevResult=document.getElementById('Result').firstChild;
						document.getElementById('Result').removeChild(prevResult);
						var result = gp2_generate_passwd(Passwd+':'+Domain,Len);
                        resultNode=document.createTextNode(result);
						document.getElementById('Result').appendChild(resultNode);
						document.getElementById('Result').style.display='block';
                        selectText('Result');

                        var kv ={
                                "Passwd":document.forms["inputForm"]["Passwd"].value,
                                "Domain":document.forms["inputForm"]["Domain"].value,
                                "Len":document.forms["inputForm"]["Len"].value,
                                "Result":result,
                                "CloseWindow":prevResult.nodeValue==resultNode.nodeValue,
                        };
                        try {
                             webkit.messageHandlers.passItAlong.postMessage(kv);
                        } catch(err) {
                            console.log('The controller does not exist yet');
                        }
					} else {
						document.getElementById('PasswdField').style.backgroundColor='#fcc';
						document.getElementById('PasswdLabel').style.color='#600';
                        document.getElementById('PasswdField').focus();
					}
				} else {
					document.getElementById('DomainField').style.backgroundColor='#fcc';
					document.getElementById('DomainLabel').style.color='#600';
                    document.getElementById('DomainField').focus();
					if (!(Passwd)) {
						document.getElementById('PasswdField').style.backgroundColor='#fcc';
						document.getElementById('PasswdLabel').style.color='#600';
                        document.getElementById('PasswdField').focus();
					}
				}

				return false;

			}
            function selectText(node) {
                node = document.getElementById(node);

                if (document.body.createTextRange) {
                    const range = document.body.createTextRange();
                    range.moveToElementText(node);
                    range.select();
                } else if (window.getSelection) {
                    const selection = window.getSelection();
                    const range = document.createRange();
                    range.selectNodeContents(node);
                    selection.removeAllRanges();
                    selection.addRange(range);
                } else {
                    console.warn("Could not select text in node: Unsupported browser.");
                }
            }

		</script>

</head>
  <body link="#00ff00">
			<form name="inputForm"  return false;" action="http://localhost:9/" method="POST">

				<fieldset id="PasswdField" class="First" onclick="document.getElementById('Passwd').focus();">

					<label id="PasswdLabel" for="Passwd">Master password</label>

					<div class="Field">
						<input id="Passwd" name="Passwd" type="password" autofocus="autofocus" value="]]

   htmlContent=htmlContent .. obj.masterPassword ..
      [[">
					</div>

				</fieldset>

				<fieldset id="DomainField" onclick="document.getElementById('Domain').focus();">

					<label id="DomainLabel" for="Domain">Domain / URL</label>

					<div class="Field">
						<input id="Domain" name="Domain" value="]]
      htmlContent=htmlContent .. obj.getAddressDefault() ..

      [[" type="url" autocorrect="off" autocapitalize="off" >
					</div>

				</fieldset>

				<fieldset id="TLDField">
					<label id="TLDLabel" for="DisableTLD">Disable subdomain removal</label>
					<input id="DisableTLD" name="DisableTLD" type="checkbox" onchange="document.getElementById('Result').style.display='none';">
				</fieldset>


				<fieldset id="LenField" class="Last" onclick="document.getElementById('Len').focus();">

					<label id="LenLabel" for="Len">Password length</label>

					<div class="Field">
						<input id="Len" name="Len" type="number" min="4" max="24" value="10" >
					</div>

				</fieldset>

				<div id="Generate">
					<a href="javascript:SGPLocal();" id="GenerateLabel" >Generate</a>
				</div>


			</form>
				<div id="Result">&#160;</div>
  </body>
</html>

]]

   local ucc = hs.webview.usercontent.new("passItAlong"):injectScript({ source = [[
    function KeyPressHappened(e)
    {
      if (!e) e=window.event;
      var code;
      if ((e.charCode) && (e.keyCode==0)) {
        code = e.charCode ;
      } else {
        code = e.keyCode;
      }
        // console.log(code) ;
      if (code == 13) {
          SGPLocal() ;
          return false ; // we handled it
      } else {
          return true ;  // we didn't handle it
      }

}
    document.onkeypress = KeyPressHappened;
    ]], mainFrame = true, injectionTime = "documentStart"}):
   setCallback(function(input)
         -- print(input.body["Passwd"])
         -- print(input.body["Domain"])
         -- print(input.body["Len"])
         -- local output, status, tp, rc = hs.execute("/Users/jixiuf/repos/dotfiles/mac/supergenpassgo" .. " -master_password " .. input.body["Passwd"] .. " -domain " .. input.body["Domain"] .. " -length " .. input.body["Len"])
         if obj.remberMasterPassword then
            obj.masterPassword=input.body["Passwd"]
         end
         result=input.body["Result"]
         if not input.body["CloseWindow"]  then
            if obj.saveGeneratedPasswordToPasteboard then
               hs.pasteboard.setContents(result)
            end
            if obj.autoComplete and obj.prevFocusedWindow ~= nil  then
               local app = obj.prevFocusedWindow:application()
               if  app:bundleID() =="com.apple.Safari" then
                  obj.safariCompletePassword(result)
               elseif  app:bundleID() =="com.google.Chrome" then
                  obj.chromeCompletePassword(result)
               elseif  app:bundleID() =="org.mozilla.firefox" then
                  obj.autoHideWindowAfterPasswordGenerated =true
               else
                  obj.prevFocusedWindow:focus()
                  hs.eventtap.keyStrokes(result)
                  obj.webview:hswindow():focus()
               end

            end
         end

         if obj.autoHideWindowAfterPasswordGenerated or input.body["CloseWindow"] then
            obj.webview:hide()
            obj.webview=nil
            obj.prevFocusedWindow:focus()
         end


         -- obj.webview:html(htmlContent)
   end)


   obj.webview = hs.webview.new({x = 500, y = 10, w = 400, h = 280}, { developerExtrasEnabled = true }, ucc)
      :windowStyle(1+2+4+8)
      :allowTextEntry(true)
      :html(htmlContent)
      :allowGestures(true)
      :closeOnEscape(true)
      :deleteOnClose(true)
      :show()

   obj.webview:bringToFront()
   obj.webview:hswindow():focus()
   if obj.autoHideWindowAfterPasswordGenerated then
      obj.webview:windowCallback(obj.windowFocusChange)
   else
      obj.webview:windowCallback(obj.windowClosing)
   end

   -- obj.webview:hswindow():centerOnScreen()
   -- obj.webview:hswindow():application():activate()


   -- obj.webview:asHSDrawing():setAlpha(1)

   -- buttonStr,masterPassword=hs.dialog.textPrompt("master_password", "info", "" )
   -- print(masterPassword)
   -- obj.setDisplay(hs.caffeinate.toggle("displayIdle"))
end

function obj.safariCompletePassword(password)
   scpt=[[
tell application "Safari"
	do JavaScript "var password = '$password'; var passwordElementsCount =0; var inputElements = document.getElementsByTagName('input'); for (var i = 0; i < inputElements.length; i++) {var inputElement = inputElements[i]; if (inputElement.type === 'password' && inputElement.value === '') {passwordElementsCount=passwordElementsCount+1; inputElement.value = password;}};passwordElementsCount; " in current tab of window 1
end tell
]]

   scpt = scpt:gsub("$password", password, 1)
   succ,output,desc=hs.osascript.applescript(scpt)
   if succ then
      if output =="0.0" or output=="0"  or output==0.0 or output==0 then -- if no password element is completed
         obj.prevFocusedWindow:focus()
         hs.eventtap.keyStrokes(result)
         obj.webview:hswindow():focus()
         -- obj.webview:windowCallback(obj.windowFocusChange)

      end
   else
      hs.dialog.blockAlert("You must enable the 'Allow JavaScript from Apple Events' option in Safari's Develop menu to use 'do JavaScript'.",serializeTable(desc))
   end
end
function obj.chromeCompletePassword(password)
   scpt=[[
tell application "Google Chrome"
	execute front window's active tab javascript  "var password = '$password'; var passwordElementsCount =0; var inputElements = document.getElementsByTagName('input'); for (var i = 0; i < inputElements.length; i++) {var inputElement = inputElements[i]; if (inputElement.type === 'password' && inputElement.value === '') {passwordElementsCount=passwordElementsCount+1; inputElement.value = password;}};passwordElementsCount; "
end tell
]]

   scpt = scpt:gsub("$password", password, 1)
   succ,output,desc=hs.osascript.applescript(scpt)
   if succ then
      if output =="0.0" or output=="0"  or output==0.0 or output==0 then -- if no password element is completed
         obj.prevFocusedWindow:focus()
         hs.eventtap.keyStrokes(result)
         obj.webview:hswindow():focus()
         -- obj.webview:windowCallback(obj.windowFocusChange)
      end
   else
      hs.dialog.blockAlert("You must enable 'Allow JavaScript from Apple Events' by going to the menu bar, View > Developer > Allow JavaScript from Apple Events",serializeTable(desc))
   end
end

return obj
