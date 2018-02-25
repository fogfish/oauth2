import React from 'react'
import { Dialog, DialogContent, DialogBody, DialogTitle, DialogSubTitle, DialogActions, Button, Link } from 'react-dress-code'
import KeyPair from './KeyPair'

const SignUp = ({onSignIn}) => (
  <Dialog>
    <DialogContent>
      <form className="form-group" action="/oauth2/signup" method="post">
        <DialogBody>
          <DialogTitle>Sign Up</DialogTitle>
          <DialogSubTitle>with</DialogSubTitle>
          <KeyPair />
        </DialogBody>
        <DialogActions with-link>
          <Link link onClick={onSignIn}>I do have the account</Link>
          <Button primary type="submit">Sign Up</Button>
        </DialogActions>
      </form>
    </DialogContent>
  </Dialog>
)

export default SignUp
