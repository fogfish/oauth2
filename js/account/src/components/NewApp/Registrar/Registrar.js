import React from 'react'
import { Button, Classes,Intent, Callout, Code } from "@blueprintjs/core";
import Identity from './Identity'
import Endpoint from './Endpoint'
import Security from './Security'
import { PENDING, FAILURE } from '../../OAuth2';


const Registrar = ({ status, ...props }) => (
  <>
    <div className={Classes.DIALOG_BODY}>
      <p>
        Registers the application with the authorization server. Define client type
        based on their ability to maintain the confidentiality of their client credentials.
        The authorization server issues the registered client a client identifier and client secret.
      </p>

      {(status.status === FAILURE && status.error.details !== 'invalid_uri') &&
        <Callout intent={Intent.DANGER}>
          Unable to register an application. Try again later.
        </Callout>
      }
      <Identity { ...props } />

      {(status.status === FAILURE && status.error.details === 'invalid_uri') &&
        <Callout intent={Intent.DANGER}>
          Invalid <strong>Redirect Uri</strong>. The schema, host and path are required.
          <Code>https://example.com/path</Code>
        </Callout>
      }
      <Endpoint { ...props } />

      <Security { ...props } />
    </div>
    <div className={Classes.DIALOG_FOOTER}>
        <div className={Classes.DIALOG_FOOTER_ACTIONS}>
            <Button
              intent={Intent.PRIMARY}
              onClick={props.commit}
              loading={status.status === PENDING}
            >
              Register
            </Button>
        </div>
    </div>
  </>
)

export default Registrar
