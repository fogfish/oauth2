var oauth2 = {}

//
// url query
oauth2.q = function(key, def, url)
{
   if (!def) def = null
   if (!url) url = window.location.href

   key = key.replace(/[\[\]]/g, "\\$&");
   var regex = new RegExp("[?&]" + key + "(=([^&#]*)|&|#|$)")
   var value = regex.exec(url);
   
   if (!value) return def;
   if (!value[2]) return '';
   return decodeURIComponent(value[2].replace(/\+/g, " "));
}