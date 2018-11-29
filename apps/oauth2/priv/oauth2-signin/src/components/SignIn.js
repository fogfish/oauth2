import React from 'react'
import { Dialog, DialogContent, DialogBody, DialogTitle, DialogSubTitle, DialogActions, Button } from 'react-dress-code'
import { Link } from 'react-router-dom'
import { KeyPair } from 'components/KeyPair'
import { GitHub } from 'components/GitHub'

const SignIn = props => (
  <Dialog>
    <DialogContent>
      <form className="form-group" action="/oauth2/signin" method="post">
        <DialogBody>
          <DialogTitle>
            <i className="fa fa-user-circle" aria-hidden="true"></i>
            &nbsp; &nbsp; Sign In
          </DialogTitle>
          {window.env.KEYPAIR && 
            <KeyPair { ...props } />  
          }
          {(window.env.KEYPAIR && window.env.KEYPAIR_RESET) && 
            <Link className="dc-link" to="/reset">Forgot Password?</Link>
          }
        </DialogBody>
        {window.env.KEYPAIR &&
          <DialogActions with-link>
            <Link className="dc-link" to="/signup"><b>Create Account</b></Link>
            <Button primary type="submit">Sign In</Button>
          </DialogActions>
        }
        {window.env.GITHUB &&
          <DialogActions>
            <GitHub { ...props } />
          </DialogActions>
        }
      </form>
    </DialogContent>
  </Dialog>
)

export default SignIn
