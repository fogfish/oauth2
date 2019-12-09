-module(oauth2_email).
-compile({parse_transform, category}).

-export([
   password_reset/2
]).

password_reset(Access, Link) ->
   try
   [either ||
      Email <- permit:as_access(Access),
      erlcloud_aws:auto_config(),
      erlcloud_ses:send_email(
         Email, 
         [{html, [
            {charset, <<"utf-8">>},
            {data, password_reset_email(Email, Link)}
         ]}], 
         "Password Recovery",
         typecast:s(os:getenv("OAUTH2_EMAIL")),
         _
      )
   ]
catch E:R ->
   serverless:error(E),
   serverless:error(R),
   ok
end.

password_reset_email(Access, Link) ->
   io_lib:format("
      <h3>Hello,</h3>
      <p>
      You recently requested to reset your password for your account <b>~s</b> at ~s. 
      Click the link below to reset it.
      </p>
      <p><a href=\"~s\">Reset Your Password</a></p>
      <p>If you did not request a password reset, please ignore this email or reply to let us know. This password reset is only valid for the next few minutes.</p> 
      <p>Thanks,</br>
      ~s team
      </p>", 
         [
            Access,
            permit_config:iss(),
            uri:s(Link),
            typecast:s(os:getenv("OAUTH2_EMAIL_SIGNATURE"))
         ]
   ).