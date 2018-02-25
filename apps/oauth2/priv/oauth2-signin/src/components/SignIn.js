import React from 'react'
import { Dialog, DialogContent, DialogBody, DialogTitle, DialogSubTitle, DialogActions, Button, Link } from 'react-dress-code'
import KeyPair from './KeyPair'
import GitHub from './GitHub'


const SignIn = ({onSignUp}) => (
  <Dialog>
    <DialogContent>
      <form className="form-group" action="/oauth2/signin" method="post">
        <DialogBody>
          <DialogTitle>Sign In</DialogTitle>
          <DialogSubTitle>with</DialogSubTitle>
          {window.env.KEYPAIR && <KeyPair />}
        </DialogBody>
        {window.env.KEYPAIR &&
          <DialogActions with-link>
            <Link link onClick={onSignUp}>Create New Account</Link>
            <Button primary type="submit">Sign In</Button>
          </DialogActions>
        }
        {window.env.GITHUB && 
          <DialogActions>
            <GitHub />
          </DialogActions>
        }
      </form>
    </DialogContent>
  </Dialog>
)

export default SignIn
