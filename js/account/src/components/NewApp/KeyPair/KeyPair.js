import React from 'react'
import { Button, Classes, Callout, Code, H5, Intent } from "@blueprintjs/core";

const Secret = ({ access, secret, hide }) => (
  <>
      <div className={Classes.DIALOG_BODY}>
        <Callout intent={Intent.WARNING}>
          This is the <strong>only</strong> time that the secret access keys can be viewed or downloaded. 
          You cannot recover them later. However, you can create new access keys at any time.
        </Callout>

        <Callout style={{marginTop: '1em'}}>
          <H5>Access Key</H5>
          <p><Code style={{overflowWrap: 'break-word'}}>{access}</Code></p>

          <H5>Secret Key</H5>
          <p><Code style={{overflowWrap: 'break-word'}}>{secret}</Code></p>
        </Callout>

      </div>
      <div className={Classes.DIALOG_FOOTER}>
          <div className={Classes.DIALOG_FOOTER_ACTIONS}>
              <Button
                intent={Intent.PRIMARY}
                onClick={() => hide()}
              >
                Ok
              </Button>
          </div>
      </div>
  </>
)

export default Secret
