import React from 'react';


export const Container = ({children}) => (
   <div className="dc-container dc-container-limited">
      {children}
   </div>
)

const Window = ({children}) => (
   <div className="dc-dialog">
      <form className="form-group" action="/oauth2/authorize" method="post"> 
         {children}
      </form>
   </div>  
)

const Dialog = ({title, subtitle, children}) => (
   <div className="dc-dialog__content">
      <div className="dc-dialog__body">
         <h3 className="dc-dialog__title">{title}</h3>
         <h4 className="dc-dialog__subtitle">{subtitle}</h4>
      </div>
   </div>
)

const Actions = ({children}) => (
   <div className="dc-dialog__actions">
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
   <a className="dc-btn dc-btn--small dc-btn--link" href="#" onClick={action}>{title}</a>
)

//
//
export const SignIn = ({onSignUp}) => (
   <Window>
      <Dialog title="Sign In" subtitle="with">
      </Dialog>
      <Actions>
         <Submit title="Sign In" />
      </Actions>
      <Links>
         <Link title="Create New Account" action={onSignUp} />
      </Links>
   </Window>
)

//
//
export const SignUp = ({onSignIn}) => (
   <Window>
      <Dialog title="Sign Up" subtitle="with">
      </Dialog>
      <Actions>
         <Submit title="Sign Up" />
      </Actions>
      <Links>
         <Link title="I do have the account." action={onSignIn} />
      </Links>
   </Window>
)

//
//
