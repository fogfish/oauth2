import React from 'react';

export const Container = ({children}) => (
   <div className="dc-container dc-container-limited">
      {children}
   </div>
)

const Window = ({url, children}) => (
   <div className="dc-dialog">
      <form className="form-group" action={url} method="post"> 
         {children}
      </form>
   </div>  
)

const Dialog = ({title, subtitle, children}) => (
   <div className="dc-dialog__content">
      <div className="dc-dialog__body">
         <h3 className="dc-dialog__title">{title}</h3>
         <h4 className="dc-dialog__subtitle">{subtitle}</h4>
         {children}
      </div>
   </div>
)

const Actions = ({children}) => (
   <div className="dc-dialog__actions">
      {children}
   </div>
)

const ActionsWithLinks = ({children}) => (
   <div className="dc-dialog__actions dc-dialog__actions--with-link">
      {children}
   </div>
)

const Submit = ({title}) => (
   <button type="submit" className="dc-btn dc-btn--primary">{title}</button>
)

const Links = ({children}) => (
   <div className="dc-dialog__actions dc-dialog__actions--secondary">
      {children}
   </div>
)

const Link = ({title, action}) => (
   <div>
      <a className="dc-btn dc-btn--small dc-btn--link" href="#" onClick={action}>{title}</a>
   </div>
)

const AccessKey = () => (
   <div>
      <label className="dc-label">
         Email <span className="dc-label__sub">required</span>
      </label>
      <input name="access" type="email" className="dc-input" autoFocus="true" required="required" />   
   </div>
)

const SecretKey = () => (
   <div>
      <label className="dc-label">
         Password <span className="dc-label__sub">required</span>
      </label>
      <input name="secret" type="password" className="dc-input" required="required" />
   </div>
)

const ClientFix = ({authRequest}) => (
   <div>
      <input name="response_type" type="hidden" value={authRequest.response_type}/>
      <input name="client_id" type="hidden" value={authRequest.client_id} />
      <input name="state" type="hidden" value={authRequest.state} />
   </div>
)

//
//
export const SignIn = ({onSignUp, authRequest}) => (
   <Window url="/oauth2/signin">
      <Dialog title="Sign In" subtitle="with">
         <AccessKey />
         <SecretKey />
         <ClientFix authRequest={authRequest}/>
      </Dialog>
      <ActionsWithLinks>
         <a className="dc-btn dc-btn--link" onClick={onSignUp}>Create New Account</a>
         <Submit title="Sign In" />
      </ActionsWithLinks>
      <Actions>
         <a className="dc-btn dc-btn" href={window.env.GITHUB + '&state=' + authRequest.client_id}> <i class="fa fa-github"></i> GitHub</a>
      </Actions>
   </Window>
)

//
//
export const SignUp = ({onSignIn, authRequest}) => (
   <Window url="/oauth2/signup">
      <Dialog title="Sign Up" subtitle="with">
         <AccessKey />
         <SecretKey />
         <ClientFix authRequest={authRequest}/>
      </Dialog>
      <ActionsWithLinks>
         <a className="dc-btn dc-btn--link" onClick={onSignIn}>I do have the account</a>
         <Submit title="Sign Up" />
      </ActionsWithLinks>
   </Window>
)

//
//
