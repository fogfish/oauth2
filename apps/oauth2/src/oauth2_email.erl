-module(oauth2_email).
-compile({parse_transform, category}).

-export([
   password_reset/2
]).

password_reset(Access, Link) ->
   [either ||
      erlcloud_aws:auto_config(),
      erlcloud_ses:send_email(
         Access, 
         [{html, [
            {charset, <<"utf-8">>},
            {data, password_reset_email(Access, Link)}
         ]}], 
         "Password Recovery",
         scalar:c(opts:val(email, oauth2)),
         _
      )
   ].

password_reset_email(Access, Link) ->
   io_lib:format("
      <h3>Hello,</h3>
      <p>
      You recently requested to reset your password for your ~s account <b>~s</b> at ~s. 
      Click the link below to reset it.
      </p>
      <p><a href=\"~s\">Reset Your Password</a></p>
      <p>If you did not request a password reset, please ignore this email or reply to let us know. This password reset is only valid for the next few minutes.</p> 

      <p>Thanks,</br>
      ~s team
      </p>", 
         [
            scalar:c(opts:val(signature, oauth2)),
            Access,
            scalar:c(opts:val(issuer, permit)),
            uri:s(Link),
            scalar:c(opts:val(signature, oauth2))
         ]
   ).


